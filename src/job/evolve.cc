/* -*- Mode: C++; tab-width: 4; indent-tabs-mode: t; c-basic-offset: 4 -*- vim:set ts=4 sw=4 sts=4 noet: */
#ifdef HAVE_CONFIG_H
#include "config.h"
#endif

#include "job.h"

#include <algorithm>
#include <cassert>
#include <chrono>
#include <cinttypes>
#include <cstdio>
#include <cstdlib>
#include <cstring>
#include <iostream>
#include <memory>
#include <random>
#include <string>

#define BOOST_DATE_TIME_NO_LIB
#include <boost/interprocess/file_mapping.hpp>
#include <boost/interprocess/mapped_region.hpp>

#include "bc.pb.h"

#include "bc/index.h"
#include "bc/locater.h"
#include "bc/mounter.h"
#include "bc/pack.h"
#include "db/read-only-driver.h"
#include "filter/cutter.h"
#include "flint/bc.h"
#include "lo/layout.h"
#include "lo/layout_loader.h"
#include "runtime/history.h"
#include "runtime/processor.h"
#include "runtime/timeseries.h"
#include "ts.h"

using std::cerr;
using std::endl;
using std::fclose;
using std::fopen;
using std::fwrite;

namespace flint {
namespace job {

namespace {

class Executor {
public:
	Executor(const Executor &) = delete;
	Executor &operator=(const Executor &) = delete;

	explicit Executor(size_t layer_size)
	: layer_size_(layer_size),
	  data_(nullptr),
	  prev_(nullptr),
	  ir_(nullptr),
	  tmp_(nullptr),
	  history_(nullptr) {}

	void set_data(double *data) {data_ = data;}
	void set_prev(double *prev) {prev_ = prev;}
	void set_ir(intptr_t *ir) {ir_ = ir;}
	void set_tmp(double *tmp) {tmp_ = tmp;}
	void set_history(History *history) {history_ = history;}

	bool Lb(const bc::Lb &lb, int offset) {
		int k = offset + lb.so();
		return history_[k].Lookback(lb, data_[kIndexTime], tmp_);
	}

	bool Load(const bc::Load &load, int offset) {
		switch (load.lo()) {
		case -1:
			tmp_[load.a()] = prev_[offset + load.so()];
			break;
		case -2:
			tmp_[load.a()] = data_[load.so()];
			break;
		default:
			{
				int k = offset + load.so() + (layer_size_ * load.lo());
				tmp_[load.a()] = data_[k];
			}
			break;
		}
		return true;
	}

	double Store(const bc::Store &store, int offset) {
		assert(store.lo() >= 0);
		int k = offset + store.so() + (layer_size_ * store.lo());
		double v = tmp_[store.a()];
		data_[k] = v;
		return v;
	}

	double *Refer(const bc::Refer &refer, int offset) {
		switch (refer.lo()) {
		case -1:
			return &prev_[offset + refer.so()];
		case -2:
			return &data_[refer.so()];
		default:
			{
				int k = offset + refer.so() + (layer_size_ * refer.lo());
				return &data_[k];
			}
		}
	}

	void Save(const bc::Save &save, int offset) {
		assert(save.lo() >= 0);
		int k = offset + save.so() + (layer_size_ * save.lo());
		std::memmove(&data_[k],
					 reinterpret_cast<double *>(ir_[save.i1()]),
					 sizeof(double)*save.k());
	}

	bool Reduce(const ReductionUnit &ru) {
		return ru(data_);
	}

private:
	size_t layer_size_;
	double *data_;
	double *prev_;
	intptr_t *ir_;
	double *tmp_;
	History *history_;
};

class PExecutor {
public:
	PExecutor(const PExecutor &) = delete;
	PExecutor &operator=(const PExecutor &) = delete;

	explicit PExecutor(size_t layer_size)
	: layer_size_(layer_size),
	  data_(nullptr),
	  prev_(nullptr),
	  color_(nullptr),
	  ir_(nullptr),
	  tmp_(nullptr),
	  history_(nullptr) {}

	void set_data(double *data) {data_ = data;}
	void set_prev(double *prev) {prev_ = prev;}
	void set_color(int *color) {color_ = color;}
	void set_ir(intptr_t *ir) {ir_ = ir;}
	void set_tmp(double *tmp) {tmp_ = tmp;}
	void set_history(History *history) {history_ = history;}

	bool Lb(const bc::Lb &lb, int offset) {
		int k = offset + lb.so();
		return history_[k].Lookback(lb, prev_[kIndexTime], tmp_);
	}

	bool Load(const bc::Load &load, int offset) {
		switch (load.lo()) {
		case -1:
			tmp_[load.a()] = prev_[offset + load.so()];
			break;
		case -2:
			tmp_[load.a()] = prev_[load.so()];
			break;
		default:
			{
				assert(false);
			}
			break;
		}
		return true;
	}

	double Store(const bc::Store &store, int offset) {
		assert(store.lo() == 0);
		int k = offset + store.so();
		double v = tmp_[store.a()];
		data_[k] = v;
		color_[k] = 1;
		return v;
	}

	double *Refer(const bc::Refer &refer, int offset) {
		switch (refer.lo()) {
		case -1:
			return &prev_[offset + refer.so()];
		case -2:
			return &prev_[refer.so()];
		default:
			assert(false);
			return nullptr;
		}
	}

	void Save(const bc::Save &save, int offset) {
		assert(save.lo() == 0);
		int k = offset + save.so();
		std::memmove(&data_[k],
					 reinterpret_cast<double *>(ir_[save.i1()]),
					 sizeof(double)*save.k());
		for (int i=0;i<save.k();i++)
			color_[k+i] = 1;
	}

	bool Reduce(const ReductionUnit &ru) {
		return ru(prev_, data_, color_);
	}

	void Flush() {
		// write back to data
		for (size_t i=kOffsetBase;i<layer_size_;i++) {
			if (color_[i]) prev_[i] = data_[i];
		}
		// clear cache and data
		std::memset(color_, 0, layer_size_ * sizeof(int));
		std::memset(data_, 0, layer_size_ * sizeof(double));
	}

private:
	size_t layer_size_;
	double *data_;
	double *prev_;
	int *color_;
	intptr_t *ir_;
	double *tmp_;
	History *history_;
};

bool SaveData(const char *output_data_file, size_t layer_size, double *data)
{
	FILE *fp = fopen(output_data_file, "wb");
	if (!fp) {
		cerr << "could not open " << output_data_file << endl;
		return false;
	}
	if (fwrite(data, sizeof(double), layer_size, fp) != layer_size) {
		fclose(fp);
		cerr << "failed to write output data" << endl;
		return false;
	}
	fclose(fp);
	return true;
}

}

bool Evolve(sqlite3 *db,
			const char *bc_file,
			const Option &option)
{
	size_t granularity = option.granularity;
	double output_start_time = option.output_start_time;
	FILE *output_fp = option.output_fp;
	FILE *stats_fp = option.stats_fp;

	bool with_filter = option.filter_file != nullptr;
	bool with_pre = option.pre_file != nullptr;
	bool with_post = option.post_file != nullptr;
	bool with_control = option.control_file != nullptr;

	// load layout at first
	std::unique_ptr<Layout> layout(new Layout);
	{
		std::unique_ptr<LayoutLoader> loader(new LayoutLoader(option.layout_file));
		if (!loader->Load(layout.get())) return false;
	}
	size_t layer_size = layout->Calculate();
	assert(layer_size > kOffsetBase);

	// load filter next
	std::unique_ptr<filter::Writer> writer;
	if (with_filter) {
		filter::Cutter cutter;
		if (!cutter.Load(option.filter_file, layer_size)) return false;
		writer.reset(cutter.CreateWriter());
	}

	std::unique_ptr<Executor> executor(new Executor(layer_size));
	std::unique_ptr<Processor> processor(new Processor(layout.get(), layer_size));
	std::unique_ptr<PExecutor> preexecutor(new PExecutor(layer_size));
	std::unique_ptr<Processor> preprocessor(new Processor(layout.get(), layer_size));
	std::unique_ptr<PExecutor> postexecutor(new PExecutor(layer_size));
	std::unique_ptr<Processor> postprocessor(new Processor(layout.get(), layer_size));

	// load bc next
	int nol = 0;
	if (!LoadBytecode(bc_file, &nol, processor.get()))
		return false;
	if (nol <= 0) {
		cerr << "invalid nol: " << nol << endl;
		return false;
	}
	// load preprocess bytecode if specified
	if (with_pre) {
		if (!LoadBytecode(option.pre_file, nullptr, preprocessor.get()))
			return false;
		// ... but ignore preprocess if empty
		if (preprocessor->IsEmpty()) {
			preprocessor.reset();
			with_pre = false;
		}
	}
	// load postprocess bytecode if specified
	if (with_post) {
		if (!LoadBytecode(option.post_file, nullptr, postprocessor.get()))
			return false;
		// ... but ignore postprocess if empty
		if (postprocessor->IsEmpty()) {
			postprocessor.reset();
			with_post = false;
		}
	}

	// replace nominal location in bytecode
	if (!processor->SolveLocation()) {
		return false;
	}
	if (with_pre && !preprocessor->SolveLocation()) {
		return false;
	}
	if (with_post && !postprocessor->SolveLocation()) {
		return false;
	}

	processor->CalculateCodeOffset();
	if (with_pre) preprocessor->CalculateCodeOffset();
	if (with_post) postprocessor->CalculateCodeOffset();

	// arrange input timeseries data
	std::unique_ptr<TimeseriesVector> tv(new TimeseriesVector);
	if (!ts::LoadTimeseriesVector(db, tv.get()))
		return false;
	processor->set_tv(tv.get());
	if (with_pre)
		preprocessor->set_tv(tv.get());
	if (with_post)
		postprocessor->set_tv(tv.get());

	{
		FlowInboundMap inbound;
		if (!LoadFlows(db, &inbound))
			return false;
		if (!processor->SolveDependencies(nol, &inbound))
			return false;
		if (with_pre)
			preprocessor->ScheduleEvents(inbound);
		if (with_post)
			postprocessor->ScheduleEvents(inbound);
	}

	// arrange previous data space
	std::unique_ptr<double[]> prev(new double[layer_size * nol]()); // default-initialized
	if (option.input_data) // fill the first layer with input
		std::memcpy(prev.get(), option.input_data->data(), layer_size * sizeof(double));
	executor->set_prev(prev.get());
	preexecutor->set_prev(prev.get());

	// arrange data space
	std::unique_ptr<double[]> data(new double[layer_size * nol]()); // default-initialized
	executor->set_data(data.get());
	postexecutor->set_prev(data.get());

	// copy time, dt, end and seed to data
	std::memcpy(data.get(), prev.get(), kOffsetBase * sizeof(double));
	// constant disposition
	{
		std::set<int> constants;
		layout->CollectConstant(1, layer_size, &constants);
		for (auto offset : constants) {
			double val = prev[offset];
			for (int i=1;i<nol;i++) { // except the 1st layer
				prev[offset + i*layer_size] = val;
			}
			for (int i=0;i<nol;i++) {
				data[offset + i*layer_size] = val;
			}
		}
	}

	// arrange work space
	std::unique_ptr<double[]> work(new double[layer_size]()); // default-initialized
	preexecutor->set_data(work.get());
	postexecutor->set_data(work.get());

	// arrange color space
	std::unique_ptr<int[]> color(new int[layer_size]()); // default-initialized
	preexecutor->set_color(color.get());
	postexecutor->set_color(color.get());

	// arrange history
	std::unique_ptr<History[]> history(new History[layer_size]);
	if (!layout->SpecifyCapacity(layer_size, history.get())) {
		return false;
	}
	if (option.input_history_file != nullptr) {
		HistoryLoader loader(option.input_history_file);
		if (!loader.Load(layer_size, history.get())) return false;
	}
	executor->set_history(history.get());
	preexecutor->set_history(history.get());
	postexecutor->set_history(history.get());

	int max_noir = processor->GetMaxNoir();
	if (with_pre)
		max_noir = std::max(max_noir, preprocessor->GetMaxNoir());
	if (with_post)
		max_noir = std::max(max_noir, postprocessor->GetMaxNoir());
	std::unique_ptr<intptr_t[]> ir(new intptr_t[max_noir]);
	executor->set_ir(ir.get());
	processor->set_ir(ir.get());
	if (with_pre) {
		preexecutor->set_ir(ir.get());
		preprocessor->set_ir(ir.get());
	}
	if (with_post) {
		postexecutor->set_ir(ir.get());
		postprocessor->set_ir(ir.get());
	}

	// calculate max number of data of block
	int max_nod = processor->GetMaxNumberOfData();
	if (with_pre) max_nod = std::max(max_nod, preprocessor->GetMaxNumberOfData());
	if (with_post) max_nod = std::max(max_nod, postprocessor->GetMaxNumberOfData());

	// initialize temporary data
	std::unique_ptr<double[]> tmp(new double[max_nod]);
	executor->set_tmp(tmp.get());
	processor->set_tmp(tmp.get());
	if (with_pre) {
		preexecutor->set_tmp(tmp.get());
		preprocessor->set_tmp(tmp.get());
	}
	if (with_post) {
		postexecutor->set_tmp(tmp.get());
		postprocessor->set_tmp(tmp.get());
	}

	// initialize pseudo random number generator
	std::unique_ptr<std::mt19937> rng(new std::mt19937(static_cast<int>(data[kIndexSeed])));
	processor->set_rng(rng.get());
	if (with_pre) preprocessor->set_rng(rng.get());
	if (with_post) postprocessor->set_rng(rng.get());

	std::unique_ptr<boost::interprocess::file_mapping> control_fm;
	std::unique_ptr<boost::interprocess::mapped_region> control_region;
	if (with_control) {
		control_fm.reset(new boost::interprocess::file_mapping(option.control_file, boost::interprocess::read_only));
		control_region.reset(new boost::interprocess::mapped_region(*control_fm, boost::interprocess::read_only));
	}
	char control;

	size_t g = (output_start_time == 0) ? 0 : granularity-1;

	bool result = true;
	int num_steps = 0;
	auto rt_start = std::chrono::steady_clock::now();
	// execute bytecode
	do {
		// push into history
		for (size_t k=0;k<layer_size; k++) {
			history[k].Insert(prev[kIndexTime], prev[k]);
		}

		// advance step
		if (with_pre) {
			if (!preprocessor->Process(preexecutor.get())) {
				result = false;
				break;
			}
			preexecutor->Flush();
		}
		if (!processor->Process(executor.get())) {
			result = false;
			break;
		}
		// one time step forward
		data[kIndexTime] += prev[kIndexDt];
		double last_time = data[kIndexTime];
		if (with_post) {
			if (!postprocessor->Process(postexecutor.get())) {
				result = false;
				break;
			}
			postexecutor->Flush();
		}

		if (output_start_time <= data[kIndexTime]) {
			if (granularity <= 1 || ++g == granularity) {
				if (with_filter) {
					if (!writer->Write(data.get(), output_fp)) {
						return false;
					}
				} else {
					// output the first layer of data
					if (fwrite(data.get(), sizeof(double), layer_size, output_fp) != layer_size) {
						cerr << "failed to write output" << endl;
						return false;
					}
				}
				g = 0;
			}
		}

		if (option.progress_address) {
			if (data[kIndexEnd] <= 0) {
				cerr << "non-positive end time: " << data[kIndexEnd] << endl;
				return false;
			}
			char c = static_cast<char>(100 * (data[kIndexTime] / data[kIndexEnd]));
			memcpy(option.progress_address, &c, 1);
		}

		if (with_control) {
			memcpy(&control, control_region->get_address(), 1);
			if (control == 1) {
				break;
			}
		}

		// update prev via double buffering
		data.swap(prev);
		executor->set_prev(prev.get());
		if (with_pre) preexecutor->set_prev(prev.get());
		executor->set_data(data.get());
		if (with_post) postexecutor->set_prev(data.get());
		data[kIndexTime] = last_time;
		++num_steps;
	} while (data[kIndexTime] < data[kIndexEnd]);

	if (stats_fp) {
		auto rt_end = std::chrono::steady_clock::now();
		auto duration = std::chrono::duration_cast<std::chrono::microseconds>(rt_end - rt_start).count();
		std::fprintf(stats_fp, "number of steps: %d\n", num_steps);
		std::fprintf(stats_fp, "duration (microseconds): %" PRId64 "\n", duration);
		if (num_steps > 0)
			std::fprintf(stats_fp, "mean (microseconds): %" PRId64 "\n", duration/num_steps);
	}

	fflush(output_fp);

	if (option.output_data_file != nullptr) {
		if (!SaveData(option.output_data_file, layer_size, data.get())) return false;
	}
	if (option.output_history_file != nullptr) {
		HistoryDumper dumper(option.output_history_file);
		if (!dumper.Dump(layer_size, history.get())) return false;
	}

	return result;
}

}
}
