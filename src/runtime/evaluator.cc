/* -*- Mode: C++; tab-width: 4; indent-tabs-mode: t; c-basic-offset: 4 -*- vim:set ts=4 sw=4 sts=4 noet: */
#ifdef HAVE_CONFIG_H
#include "config.h"
#endif

#include "runtime/evaluator.h"

#include <cassert>
#include <cstdio>
#include <cstdlib>
#include <cstring>
#include <iostream>
#include <map>
#include <memory>
#include <random>

#include <boost/uuid/uuid_io.hpp>

#include "bc.pb.h"

#include "bc/index.h"
#include "bc/locater.h"
#include "bc/mounter.h"
#include "db/sprinkle-loader.h"
#include "flint/bc.h"
#include "lo/layout.h"
#include "lo/layout_loader.h"
#include "runtime/processor.h"
#include "runtime/timeseries.h"
#include "ts.h"

namespace flint {
namespace runtime {

namespace {

class TargetHandler {
public:
	TargetHandler(const TargetHandler &) = delete;
	TargetHandler &operator=(const TargetHandler &) = delete;

	TargetHandler(DataOffsetMap *dom, SectorOffsetMap *som, double *data, bool *target)
		: dom_(dom),
		  som_(som),
		  data_(data),
		  target_(target)
	{}

	bool Handle(const boost::uuids::uuid &track_id,
				const boost::uuids::uuid &sector_id,
				int pq_id,
				double value) {
		DataOffsetMap::const_iterator domit = dom_->find(track_id);
		if (domit == dom_->end()) {
			std::cerr << "unknown track: " << track_id << std::endl;
			return false;
		}
		std::unordered_map<int, int>::const_iterator pqit = domit->second.find(pq_id);
		if (pqit == domit->second.end()) {
			std::cerr << "unknow physical-quantity-id: " << pq_id << std::endl;
			return false;
		}
		SectorOffsetMap::const_iterator somit = som_->find(sector_id);
		if (somit == som_->end()) {
			std::cerr << "unknown sector: " << sector_id << std::endl;
			return false;
		}
		int offset = somit->second + pqit->second;
		data_[offset] = value; // TODO
		target_[offset] = true;
		return true;
	}

private:
	DataOffsetMap *dom_;
	SectorOffsetMap *som_;
	double *data_;
	bool *target_;
};

class Executor {
public:
	Executor(const Executor &) = delete;
	Executor &operator=(const Executor &) = delete;

	Executor()
	: data_(nullptr),
	  ir_(nullptr),
	  tmp_(nullptr),
	  target_(nullptr),
	  color_(nullptr)
	{}

	void set_data(double *data) {data_ = data;}
	void set_ir(intptr_t *ir) {ir_ = ir;}
	void set_tmp(double *tmp) {tmp_ = tmp;}
	void set_target(bool *target) {target_ = target;}
	void set_color(size_t *color) {color_ = color;}

	bool Lb(const bc::Lb &lb, int offset) {
		// no history
		int k = offset + lb.so();
		tmp_[lb.a()] = data_[k];
		return true;
	}

	bool Load(const bc::Load &load, int offset) {
		switch (load.lo()) {
		case -1:
			assert(false);
			break;
		case -2:
			tmp_[load.a()] = data_[load.so()];
			break;
		default:
			{
				assert(load.lo() == 0);
				int k = offset + load.so();
				tmp_[load.a()] = data_[k];
			}
			break;
		}
		return true;
	}

	double Store(const bc::Store &store, int offset) {
		assert(store.lo() == 0);
		int k = offset + store.so();
		double v = tmp_[store.a()];
		if (!target_[k]) { // unless the location is targeted as overwritten
			data_[k] = v;
		}
		color_[k] = 1;
		return v;
	}

	double *Refer(const bc::Refer &refer, int offset) {
		switch (refer.lo()) {
		case -1:
			assert(false);
			return nullptr;
		case -2:
			return &data_[refer.so()];
		default:
			{
				assert(refer.lo() == 0);
				int k = offset + refer.so();
				return &data_[k];
			}
		}
	}

	void Save(const bc::Save &save, int offset) {
		assert(save.lo() == 0);
		int k = offset + save.so();
		if (!target_[k]) { // unless the location is targeted as overwritten
			std::memmove(&data_[k],
						 reinterpret_cast<double *>(ir_[save.i1()]),
						 sizeof(double)*save.k());
		}
		for (int i=0;i<save.k();i++)
			color_[k+i] = 1;
	}

	bool Reduce(const ReductionUnit &ru) {
		if (!ru(data_))
			return false;
		size_t n = ru.source_addrs().size();
		color_[ru.target_addr()] = (n > 0) ? n : 1u;
		return true;
	}

private:
	double *data_;
	intptr_t *ir_;
	double *tmp_;
	bool *target_;
	size_t *color_;
};

bool IsAllGreen(size_t *color, size_t size)
{
	for (size_t i=kOffsetBase;i<size;i++) {
		if (!color[i]) return false;
	}
	return true;
}

} // namespace

bool Evaluator::Load(const boost::filesystem::path &layout_file)
{
	LayoutLoader loader(layout_file);
	if (!loader.Load(&layout_))
		return false;
	layer_size_ = layout_.Calculate(&dom_, &som_);
	if (layer_size_ == kOffsetBase) {
		std::cerr << "no variables found, possibly due to empty model or no instances" << std::endl;
		return false;
	}
	return true;
}

bool Evaluator::Evaluate(sqlite3 *db,
						 ct::Availability availability,
						 int seed,
						 Bytecode *bytecode,
						 const FlowInboundMap *inbound,
						 const TimeseriesVector *tv,
						 std::vector<double> *data)
{
	std::unique_ptr<Executor> executor(new Executor);
	std::unique_ptr<Processor> processor(new Processor(&layout_, layer_size_, bytecode, tv));

	// arrange data space
	data->resize(layer_size_);
	executor->set_data(data->data());

	// initialize color space
	std::unique_ptr<size_t[]> color(new size_t[layer_size_]()); // default-initialized
	executor->set_color(color.get());

	std::unique_ptr<bool[]> target(new bool[layer_size_]()); // default-initialized
	executor->set_target(target.get());

	// read targets from database, if any
	{
		db::SprinkleLoader loader(db);
		std::unique_ptr<TargetHandler> handler(new TargetHandler(&dom_, &som_, data->data(), target.get()));
		if (!loader.Load(handler.get())) {
			return false;
		}
	}

	int nol = bytecode->nol;
	if (nol != 1) { // nol should be always 1
		std::cerr << "invalid nol: " << nol << std::endl;
		return false;
	}

	// replace nominal location in bytecode
	if (!processor->SolveLocation()) {
		return false;
	}

	processor->CalculateCodeOffset();

	if (!processor->SolveDependencies(inbound, availability, color.get()))
		return false;

	int max_noir = processor->GetMaxNoir();
	std::unique_ptr<intptr_t[]> ir(new intptr_t[max_noir]);
	executor->set_ir(ir.get());
	processor->set_ir(ir.get());

	// calculate max number of data of block
	int max_nod = processor->GetMaxNumberOfData();

	// initialize temporary data
	std::unique_ptr<double[]> tmp(new double[max_nod]);
	executor->set_tmp(tmp.get());
	processor->set_tmp(tmp.get());

	// initialize pseudo random number generator with given seed
	std::unique_ptr<std::mt19937> rng(new std::mt19937(seed));
	(*data)[kIndexSeed] = static_cast<double>(seed);
	processor->set_rng(rng.get());

	if (!processor->Process(executor.get())) return false;

	if (!IsAllGreen(color.get(), layer_size_)) {
		std::cerr << "could not calculate initial values of some variables:" << std::endl;
		layout_.DetectRed(layer_size_, color.get());
		return false;
	}
	return true;
}

}
}
