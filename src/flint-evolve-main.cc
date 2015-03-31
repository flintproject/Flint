/* -*- Mode: C++; tab-width: 4; indent-tabs-mode: t; c-basic-offset: 4 -*- vim:set ts=4 sw=4 sts=4 noet: */
#ifdef HAVE_CONFIG_H
#include "config.h"
#endif

#include <cassert>
#include <cstdio>
#include <cstdlib>
#include <iostream>
#include <string>

#define BOOST_DATE_TIME_NO_LIB
#include <boost/interprocess/file_mapping.hpp>
#include <boost/interprocess/mapped_region.hpp>
#include <boost/noncopyable.hpp>
#include <boost/program_options.hpp>
#include <boost/random.hpp>
#include <boost/scoped_array.hpp>
#include <boost/scoped_ptr.hpp>

#include "bc.pb.h"

#include "bc/bc_loader.h"
#include "bc/index.h"
#include "bc/locater.h"
#include "bc/mounter.h"
#include "bc/pack.h"
#include "filter/cutter.h"
#include "lo/layout.h"
#include "lo/layout_loader.h"
#include "runtime/history.h"
#include "runtime/processor.h"
#include "runtime/timeseries.h"
#include "text/flow_loader.h"

namespace po = boost::program_options;

using std::cerr;
using std::endl;
using std::fclose;
using std::fopen;
using std::fputc;
using std::fread;
using std::fwrite;
using std::string;

namespace {

class Executor : boost::noncopyable {
public:
	explicit Executor(size_t layer_size)
	: layer_size_(layer_size),
	  data_(NULL),
	  prev_(NULL),
	  tmp_(NULL),
	  history_(NULL) {}

	void set_data(double *data) {data_ = data;}
	void set_prev(double *prev) {prev_ = prev;}
	void set_tmp(double *tmp) {tmp_ = tmp;}
	void set_history(History *history) {history_ = history;}

	bool Lb(const bc::Lb &lb, int offset) {
		int k = offset + lb.so();
		return history_[k].Lookback(lb, tmp_);
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

	double Store(const bc::Store &store, int offset, const std::set<int> &flow_addrs) {
		assert(store.lo() >= 0);
		int k = offset + store.so() + (layer_size_ * store.lo());
		double v = tmp_[store.a()];
		data_[k] = v;

		// send stored value through flows
		// FIXME: sum only
		for (std::set<int>::const_iterator it=flow_addrs.begin();it!=flow_addrs.end();++it) {
			data_[*it] += data_[k];
		}

		return v;
	}

private:
	size_t layer_size_;
	double *data_;
	double *prev_;
	double *tmp_;
	History *history_;
};

class PExecutor : boost::noncopyable {
public:
	explicit PExecutor(size_t layer_size)
	: layer_size_(layer_size),
	  data_(NULL),
	  prev_(NULL),
	  color_(NULL),
	  tmp_(NULL),
	  history_(NULL) {}

	void set_data(double *data) {data_ = data;}
	void set_prev(double *prev) {prev_ = prev;}
	void set_color(int *color) {color_ = color;}
	void set_tmp(double *tmp) {tmp_ = tmp;}
	void set_history(History *history) {history_ = history;}

	bool Lb(const bc::Lb &lb, int offset) {
		int k = offset + lb.so();
		return history_[k].Lookback(lb, tmp_);
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

	void Communicate(const FlowInboundMap *inbound) {
		for (FlowInboundMap::const_iterator it=inbound->begin();it!=inbound->end();++it) {
			int dst = it->first;
			for (std::set<int>::const_iterator tit=it->second->begin();tit!=it->second->end();++tit) {
				int src = *tit;
				if (color_[src]) {
					data_[dst] += data_[src];
				} else {
					data_[dst] += prev_[src];
				}
			}
			color_[dst] = 1;
		}
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
	double *tmp_;
	History *history_;
};

bool CreateStatusFile(const string &status_file)
{
	FILE *fp = fopen(status_file.c_str(), "wb");
	if (!fp) {
		cerr << "could not open " << status_file << endl;
		return false;
	}
	if (fputc('\0', fp) == EOF) {
		fclose(fp);
		cerr << "failed to write byte to " << status_file << endl;
		return false;
	}
	fclose(fp);
	return true;
}

bool LoadData(const string &input_data_file, size_t layer_size, double *data)
{
	FILE *fp = fopen(input_data_file.c_str(), "rb");
	if (!fp) {
		cerr << "could not open " << input_data_file << endl;
		return false;
	}
	if (fread(data, sizeof(double), layer_size, fp) != layer_size) {
		fclose(fp);
		cerr << "missing or short input data" << endl;
		return false;
	}
	fclose(fp);
	return true;
}

bool SaveData(const string &output_data_file, size_t layer_size, double *data)
{
	FILE *fp = fopen(output_data_file.c_str(), "wb");
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

} // namespace

int main(int argc, char *argv[])
{
	GOOGLE_PROTOBUF_VERIFY_VERSION;

	po::options_description opts("options");
	po::positional_options_description popts;
	po::variables_map vm;
	string layout_file, bc_file, pre_file, post_file, flow_file, filter_file;
	string input_data_file, output_data_file;
	string input_history_file, output_history_file;
	string input_timeseries_file;
	string control_file, status_file;
	size_t granularity = 0;
	string output_file;
	int print_help = 0;

	opts.add_options()
		("layout", po::value<string>(&layout_file), "Input layout file")
		("bc", po::value<string>(&bc_file), "Input bytecode file")
		("pre", po::value<string>(&pre_file), "Input preprocess bytecode file")
		("post", po::value<string>(&post_file), "Input postprocess bytecode file")
		("flow", po::value<string>(&flow_file), "Input flow file")
		("filter", po::value<string>(&filter_file), "Input filter file")
		("input-data", po::value<string>(&input_data_file), "Input data file")
		("input-history", po::value<string>(&input_history_file), "Input history file")
		("input-timeseries", po::value<string>(&input_timeseries_file), "Input timeseries file")
		("control", po::value<string>(&control_file), "Input control file")
		("output-data", po::value<string>(&output_data_file), "Output data file")
		("output-history", po::value<string>(&output_history_file), "Output history file")
		("status", po::value<string>(&status_file), "Output status file")
		("granularity", po::value<size_t>(&granularity), "Granularity")
		("output", po::value<string>(&output_file), "Output file")
		("help,h", "Show this message");
	popts.add("layout", 1).add("bc", 1).add("flow", 1).add("output", 1);

	try {
		po::store(po::command_line_parser(argc, argv).options(opts).positional(popts).run(), vm);
		po::notify(vm);
		if (vm.count("help") > 0) {
			print_help = 1;
		} else if ( vm.count("layout") == 0 ||
					vm.count("bc") == 0 ||
					vm.count("flow") == 0 ||
					vm.count("output") == 0 ) {
			print_help = 2;
		}
	} catch (const po::error &) {
		print_help = 2;
	}
	if (print_help) {
		cerr << "usage: " << argv[0] << " LAYOUT BC FLOW OUTPUT" << endl;
		cerr << opts;
		return (print_help == 1) ? EXIT_SUCCESS : EXIT_FAILURE;
	}

	bool with_filter = vm.count("filter") > 0;
	bool with_pre = vm.count("pre") > 0;
	bool with_post = vm.count("post") > 0;
	bool with_control = vm.count("control") > 0;
	bool with_status = vm.count("status") > 0;

	// load layout at first
	boost::scoped_ptr<Layout> layout(new Layout);
	{
		boost::scoped_ptr<LayoutLoader> loader(new LayoutLoader(layout_file));
		if (!loader->Load(layout.get())) return EXIT_FAILURE;
	}
	size_t layer_size = layout->Calculate();

	// load filter next
	boost::scoped_ptr<Cutter> cutter(new Cutter);
	if (with_filter) {
		if (!cutter->Load(filter_file.c_str(), layer_size)) return EXIT_FAILURE;
	}

	boost::scoped_ptr<Executor> executor(new Executor(layer_size));
	boost::scoped_ptr<Processor> processor(new Processor(layout.get(), layer_size));
	boost::scoped_ptr<PExecutor> preexecutor(new PExecutor(layer_size));
	boost::scoped_ptr<Processor> preprocessor(new Processor(layout.get(), layer_size));
	boost::scoped_ptr<PExecutor> postexecutor(new PExecutor(layer_size));
	boost::scoped_ptr<Processor> postprocessor(new Processor(layout.get(), layer_size));

	// load bc next
	int nol = 0;
	{
		boost::scoped_ptr<BcLoader> loader(new BcLoader(bc_file));
		if (!loader->Load(&nol, processor.get())) return EXIT_FAILURE;
	}
	if (nol <= 0) {
		cerr << "invalid nol: " << nol << endl;
		return EXIT_FAILURE;
	}
	// load preprocess bytecode if specified
	if (with_pre) {
		boost::scoped_ptr<BcLoader> loader(new BcLoader(pre_file));
		if (!loader->Load(NULL, preprocessor.get())) return EXIT_FAILURE;
	}
	// load postprocess bytecode if specified
	if (with_post) {
		boost::scoped_ptr<BcLoader> loader(new BcLoader(post_file));
		if (!loader->Load(NULL, postprocessor.get())) return EXIT_FAILURE;
	}

	processor->CalculateCodeOffset();
	if (with_pre) preprocessor->CalculateCodeOffset();
	if (with_post) postprocessor->CalculateCodeOffset();

	// replace nominal location in bytecode
	if (!processor->SolveLocation()) {
		return EXIT_FAILURE;
	}
	if (with_pre && !preprocessor->SolveLocation()) {
		return EXIT_FAILURE;
	}
	if (with_post && !postprocessor->SolveLocation()) {
		return EXIT_FAILURE;
	}

	// load flow
	boost::scoped_ptr<FlowInboundMap> inbound(new FlowInboundMap);
	boost::scoped_ptr<FlowOutboundMap> outbound(new FlowOutboundMap);
	{
		boost::scoped_ptr<text::FlowLoader> loader(new text::FlowLoader(flow_file));
		boost::scoped_ptr<FlowHandler> handler(new FlowHandler(inbound.get(),
															   outbound.get()));
		if (!loader->Load(handler.get())) return EXIT_FAILURE;
	}

	if (!processor->SolveDependencies(nol, inbound.get(), outbound.get(), true)) {
		return EXIT_FAILURE;
	}

	// arrange previous data space
	boost::scoped_array<double> prev(new double[layer_size]()); // default-initialized
	if (vm.count("input-data") > 0) { // fill the first layer with input
		if (!LoadData(input_data_file, layer_size, prev.get())) return EXIT_FAILURE;
	}
	executor->set_prev(prev.get());
	preexecutor->set_prev(prev.get());

	// arrange data space
	boost::scoped_array<double> data(new double[layer_size * nol]()); // default-initialized
	executor->set_data(data.get());
	postexecutor->set_prev(data.get());

	// arrange disposition
	boost::scoped_array<double> cdata(new double[layer_size]());
	boost::scoped_array<size_t> disposition(new size_t[layer_size]()); // default-initialized
	processor->SolveConstantDisposition(outbound.get(),
										prev.get(),
										cdata.get(),
										disposition.get());

	// copy time, dt, end and seed to data
	std::memcpy(data.get(), prev.get(), kOffsetBase * sizeof(double));
	// disposition
	for (size_t offset=kOffsetBase;offset<layer_size;offset++) {
		if (disposition[offset]) {
			for (int i=0;i<nol;i++) {
				data[offset + i*layer_size] = cdata[offset];
			}
		}
	}

	// arrange work space
	boost::scoped_array<double> work(new double[layer_size]()); // default-initialized
	preexecutor->set_data(work.get());
	postexecutor->set_data(work.get());

	// arrange color space
	boost::scoped_array<int> color(new int[layer_size]()); // default-initialized
	preexecutor->set_color(color.get());
	postexecutor->set_color(color.get());

	// arrange history
	boost::scoped_array<History> history(new History[layer_size]);
	if (!layout->SpecifyCapacity(layer_size, history.get())) {
		return EXIT_FAILURE;
	}
	if (vm.count("input-history") > 0) {
		HistoryLoader loader(input_history_file.c_str());
		if (!loader.Load(layer_size, history.get())) return EXIT_FAILURE;
	}
	executor->set_history(history.get());
	preexecutor->set_history(history.get());
	postexecutor->set_history(history.get());

	std::set<int> vo;
	layout->CollectVariable(layer_size, &vo);

	// arrange input timeseries data
	boost::scoped_ptr<TimeseriesVector> tv;
	if (vm.count("input-timeseries")) {
		tv.reset(new TimeseriesVector);
		TimeseriesLoader loader(input_timeseries_file.c_str());
		if (!loader.Load(tv.get())) return EXIT_FAILURE;
		processor->set_tv(tv.get());
		if (with_pre) preprocessor->set_tv(tv.get());
		if (with_post) postprocessor->set_tv(tv.get());
	}

	// calculate max number of data of block
	int max_nod = processor->GetMaxNumberOfData();
	if (with_pre) max_nod = std::max(max_nod, preprocessor->GetMaxNumberOfData());
	if (with_post) max_nod = std::max(max_nod, postprocessor->GetMaxNumberOfData());

	// initialize temporary data
	boost::scoped_array<double> tmp(new double[max_nod]);
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
	boost::scoped_ptr<boost::mt19937> rng(new boost::mt19937(static_cast<int>(data[kIndexSeed])));
	processor->set_rng(rng.get());
	if (with_pre) preprocessor->set_rng(rng.get());
	if (with_post) postprocessor->set_rng(rng.get());

	boost::scoped_ptr<boost::interprocess::file_mapping> control_fm;
	boost::scoped_ptr<boost::interprocess::mapped_region> control_region;
	if (with_control) {
		control_fm.reset(new boost::interprocess::file_mapping(control_file.c_str(), boost::interprocess::read_only));
		control_region.reset(new boost::interprocess::mapped_region(*control_fm, boost::interprocess::read_only));
	}
	char control;

	if (with_status) {
		if (!CreateStatusFile(status_file)) return EXIT_FAILURE;
	}
	char p, q = 0;

	size_t g = 0;

	FILE *output_fp = fopen(output_file.c_str(), "ab");
	if (!output_fp) {
		std::perror(output_file.c_str());
		return EXIT_FAILURE;
	}

	int result = EXIT_SUCCESS;
	// execute bytecode
	do {
		// push into history
		for (size_t k=0;k<layer_size; k++) {
			history[k].Insert(prev[kIndexTime], prev[k]);
		}

		// advance step
		if (with_pre) {
			if (!preprocessor->Execute(preexecutor.get(), inbound.get())) {
				result = EXIT_FAILURE;
				break;
			}
		}
		if (!processor->Process(executor.get())) {
			result = EXIT_FAILURE;
			break;
		}
		// one time step forward
		data[kIndexTime] += prev[kIndexDt];
		if (with_post) {
			if (!postprocessor->Execute(postexecutor.get(), inbound.get())) {
				result = EXIT_FAILURE;
				break;
			}
		}

		// update prev, but keep end, dt, seed, and constant values
		prev[kIndexTime] = data[kIndexTime];
		for (std::set<int>::const_iterator vit=vo.begin();vit!=vo.end();++vit) {
			prev[*vit] = data[*vit];
		}
		if (std::memcmp(prev.get()+1, data.get()+1, (layer_size-1)*sizeof(double)) != 0) {
			cerr << "layer_size: " << layer_size << endl;
			cerr << "prev:" << endl;
			for (size_t i=0;i<layer_size;i++) {
				cerr << "[" << i << "]" << prev[i] << " ";
			}
			cerr << endl;
			cerr << "data:" << endl;
			for (size_t i=0;i<layer_size;i++) {
				cerr << "[" << i << "]" << data[i] << " ";
			}
			cerr << endl;
			return EXIT_FAILURE;
		}

		if (granularity <= 1 || ++g == granularity) {
			if (with_filter) {
				if (!cutter->Apply(data.get(), output_fp)) {
					return EXIT_FAILURE;
				}
			} else {
				// output the first layer of data
				if (fwrite(data.get(), sizeof(double), layer_size, output_fp) != layer_size) {
					cerr << "failed to write output" << endl;
					return EXIT_FAILURE;
				}
			}
			g = 0;
		}

		if (with_status) {
			if (data[kIndexEnd] <= 0) {
				cerr << "non-positive end time: " << data[kIndexEnd] << endl;
				return EXIT_FAILURE;
			}
			p = static_cast<char>(100 * (data[kIndexTime] / data[kIndexEnd]));
			if (p != q) {
				FILE *fp = fopen(status_file.c_str(), "wb");
				if (fp) {
					fwrite(&p, 1, 1, fp);
					fclose(fp);
				}
				q = p;
			}
		}

		if (with_control) {
			memcpy(&control, control_region->get_address(), 1);
			if (control == 1) {
				break;
			}
		}

		// clear data
		std::memset(data.get()+kOffsetBase, 0, (nol*layer_size-kOffsetBase)*sizeof(double));
		// disposition
		for (size_t offset=kOffsetBase;offset<layer_size;offset++) {
			if (disposition[offset]) {
				for (int i=0;i<nol;i++) {
					data[offset + i*layer_size] = cdata[offset];
				}
			}
		}

	} while (data[kIndexTime] < data[kIndexEnd]);
	fclose(output_fp);

	if (vm.count("output-data") > 0) {
		if (!SaveData(output_data_file, layer_size, data.get())) return EXIT_FAILURE;
	}
	if (vm.count("output-history") > 0) {
		HistoryDumper dumper(output_history_file.c_str());
		if (!dumper.Dump(layer_size, history.get())) return EXIT_FAILURE;
	}

	google::protobuf::ShutdownProtobufLibrary();

	return result;
}
