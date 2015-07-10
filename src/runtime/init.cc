/* -*- Mode: C++; tab-width: 4; indent-tabs-mode: t; c-basic-offset: 4 -*- vim:set ts=4 sw=4 sts=4 noet: */
#ifdef HAVE_CONFIG_H
#include "config.h"
#endif

#include "runtime.hh"

#include <cassert>
#include <cstdio>
#include <cstdlib>
#include <iostream>
#include <map>

#include <boost/noncopyable.hpp>
#include <boost/random.hpp>
#include <boost/scoped_array.hpp>
#include <boost/scoped_ptr.hpp>
#include <boost/uuid/uuid_io.hpp>

#include "bc.pb.h"

#include "bc/bc_loader.h"
#include "bc/index.h"
#include "bc/locater.h"
#include "bc/mounter.h"
#include "bc/pack.h"
#include "db/sprinkle-loader.h"
#include "lo/layout.h"
#include "lo/layout_loader.h"
#include "runtime/processor.h"
#include "runtime/timeseries.h"
#include "ts.hh"

using std::cerr;
using std::endl;
using std::fclose;
using std::fopen;
using std::fwrite;

namespace runtime {

namespace {

class TargetHandler : boost::noncopyable {
public:
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
			cerr << "unknown track: " << track_id << endl;
			return false;
		}
		std::unordered_map<int, int>::const_iterator pqit = domit->second->find(pq_id);
		if (pqit == domit->second->end()) {
			cerr << "unknow physical-quantity-id: " << pq_id << endl;
			return false;
		}
		SectorOffsetMap::const_iterator somit = som_->find(sector_id);
		if (somit == som_->end()) {
			cerr << "unknown track: " << sector_id << endl;
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

class Executor : boost::noncopyable {
public:
	explicit Executor(size_t layer_size)
	: layer_size_(layer_size),
	  data_(NULL),
	  tmp_(NULL),
	  target_(NULL),
	  color_(NULL)
	{}

	void set_data(double *data) {data_ = data;}
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

	double Store(const bc::Store &store, int offset, const std::unordered_set<int> &flow_addrs) {
		assert(store.lo() == 0);
		int k = offset + store.so();
		double v = tmp_[store.a()];
		if (!target_[k]) { // unless the location is targeted as overwritten
			data_[k] = v;
		}
		color_[k] = 1;

		// send stored value through flows
		// FIXME: sum only
		for (int a : flow_addrs) {
			data_[a] += data_[k];
			color_[a]++;
		}

		return v;
	}

private:
	size_t layer_size_;
	double *data_;
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

bool Init(sqlite3 *db, const char *layout_file, const char *bc_file, const char *output_file)
{
	// load layout at first
	boost::scoped_ptr<Layout> layout(new Layout);
	{
		boost::scoped_ptr<LayoutLoader> loader(new LayoutLoader(layout_file));
		if (!loader->Load(layout.get())) return false;
	}
	boost::scoped_ptr<DataOffsetMap> dom(new DataOffsetMap);
	boost::scoped_ptr<SectorOffsetMap> som(new SectorOffsetMap);
	size_t layer_size = layout->Calculate(dom.get(), som.get());

	boost::scoped_ptr<Executor> executor(new Executor(layer_size));
	boost::scoped_ptr<Processor> processor(new Processor(layout.get(), layer_size));

	// arrange data space
	boost::scoped_array<double> data(new double[layer_size]()); // default-initialized
	executor->set_data(data.get());

	boost::scoped_array<bool> target(new bool[layer_size]()); // default-initialized
	executor->set_target(target.get());

	// arrange input timeseries data
	boost::scoped_ptr<TimeseriesVector> tv;

	boost::scoped_ptr<FlowInboundMap> inbound(new FlowInboundMap);
	boost::scoped_ptr<FlowOutboundMap> outbound(new FlowOutboundMap);

	// read targets from database, if any
	{
		db::SprinkleLoader loader(db);
		boost::scoped_ptr<TargetHandler> handler(new TargetHandler(dom.get(), som.get(), data.get(), target.get()));
		if (!loader.Load(handler.get())) {
			return false;
		}
	}
	{
		tv.reset(new TimeseriesVector);
		if (!ts::LoadTimeseriesVector(db, tv.get()))
			return false;
		processor->set_tv(tv.get());
	}
	if (!LoadFlows(db, inbound.get(), outbound.get()))
		return false;

	// load bc next
	int nol = 0;
	{
		boost::scoped_ptr<BcLoader> loader(new BcLoader(bc_file));
		if (!loader->Load(&nol, processor.get())) return false;
	}
	if (nol != 1) { // nol should be always 1
		cerr << "invalid nol: " << nol << endl;
		return false;
	}

	processor->CalculateCodeOffset();

	// replace nominal location in bytecode
	if (!processor->SolveLocation()) {
		return false;
	}

	if (!processor->SolveDependencies(nol, inbound.get(), outbound.get(), false)) return false;

	// calculate max number of data of block
	int max_nod = processor->GetMaxNumberOfData();

	// initialize temporary data
	boost::scoped_array<double> tmp(new double[max_nod]);
	executor->set_tmp(tmp.get());
	processor->set_tmp(tmp.get());

	// initialize color space
	boost::scoped_array<size_t> color(new size_t[layer_size]()); // default-initialized
	executor->set_color(color.get());

	// initialize pseudo random number generator
	// FIXME: specify the seed given by user
	int seed = static_cast<int>(time(NULL));
	data[kIndexSeed] = seed;
	boost::scoped_ptr<boost::mt19937> rng(new boost::mt19937(seed));
	processor->set_rng(rng.get());

	if (!processor->Process(executor.get())) return false;

	if (!IsAllGreen(color.get(), layer_size)) {
		cerr << "could not calculate initial values of some variables:" << endl;
		layout->DetectRed(layer_size, color.get());
		return false;
	}

	FILE *fp = fopen(output_file, "wb");
	if (!fp) {
		std::perror(output_file);
		return false;
	}
	// output the first layer of data
	if (fwrite(data.get(), sizeof(double), layer_size, fp) != layer_size) {
		cerr << "failed to write output: " << output_file << endl;
		fclose(fp);
		return false;
	}
	fclose(fp);

	return true;
}

}
