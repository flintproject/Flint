/* -*- Mode: C++; tab-width: 4; indent-tabs-mode: t; c-basic-offset: 4 -*- vim:set ts=4 sw=4 sts=4 noet: */
#ifdef HAVE_CONFIG_H
#include "config.h"
#endif

#include <cassert>
#include <cstdio>
#include <cstdlib>
#include <iostream>
#include <map>
#include <string>

#include <boost/noncopyable.hpp>
#include <boost/program_options.hpp>
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
#include "db/read-only-driver.hh"
#include "db/sprinkle-loader.h"
#include "filter/cutter.h"
#include "lo/layout.h"
#include "lo/layout_loader.h"
#include "runtime/processor.h"
#include "runtime/timeseries.h"
#include "ts.hh"

namespace po = boost::program_options;

using std::cerr;
using std::endl;
using std::fclose;
using std::fopen;
using std::fputc;
using std::fread;
using std::fwrite;
using std::make_pair;
using std::memcpy;
using std::string;

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
		std::map<int, int>::const_iterator pqit = domit->second->find(pq_id);
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

	double Store(const bc::Store &store, int offset, const std::set<int> &flow_addrs) {
		assert(store.lo() == 0);
		int k = offset + store.so();
		double v = tmp_[store.a()];
		if (!target_[k]) { // unless the location is targeted as overwritten
			data_[k] = v;
		}
		color_[k] = 1;

		// send stored value through flows
		// FIXME: sum only
		for (std::set<int>::const_iterator it=flow_addrs.begin();it!=flow_addrs.end();++it) {
			data_[*it] += data_[k];
			color_[*it]++;
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

int main(int argc, char *argv[])
{
	GOOGLE_PROTOBUF_VERIFY_VERSION;

	po::options_description opts("options");
	po::positional_options_description popts;
	po::variables_map vm;
	string layout_file, bc_file, filter_file, output_file;
	string db_file;
	int print_help = 0;

	opts.add_options()
		("layout", po::value<string>(&layout_file), "Input layout file")
		("bc", po::value<string>(&bc_file), "Input bytecode file")
		("filter", po::value<string>(&filter_file), "Input filter file")
		("db", po::value<string>(&db_file), "Input database file")
		("output", po::value<string>(&output_file), "Output file")
		("help,h", "Show this message");
	popts.add("db", 1).add("layout", 1).add("bc", 1).add("output", 1);

	try {
		po::store(po::command_line_parser(argc, argv).options(opts).positional(popts).run(), vm);
		po::notify(vm);
		if (vm.count("help") > 0) {
			print_help = 1;
		} else if ( vm.count("db") == 0 ||
					vm.count("layout") == 0 ||
					vm.count("bc") == 0 ||
					vm.count("output") == 0 ) {
			print_help = 2;
		}
	} catch (const po::error &) {
		print_help = 2;
	}
	if (print_help) {
		cerr << "usage: " << argv[0] << " DB LAYOUT BC OUTPUT" << endl;
		cerr << opts;
		return (print_help == 1) ? EXIT_SUCCESS : EXIT_FAILURE;
	}

	bool with_filter = vm.count("filter") > 0;

	// load layout at first
	boost::scoped_ptr<Layout> layout(new Layout);
	{
		boost::scoped_ptr<LayoutLoader> loader(new LayoutLoader(layout_file));
		if (!loader->Load(layout.get())) return EXIT_FAILURE;
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
		db::ReadOnlyDriver driver(db_file.c_str());
		{
			db::SprinkleLoader loader(driver.db());
			boost::scoped_ptr<TargetHandler> handler(new TargetHandler(dom.get(), som.get(), data.get(), target.get()));
			if (!loader.Load(handler.get())) {
				return EXIT_FAILURE;
			}
		}
		{
			tv.reset(new TimeseriesVector);
			if (!ts::LoadTimeseriesVector(driver.db(), tv.get()))
				return EXIT_FAILURE;
			processor->set_tv(tv.get());
		}
		if (!LoadFlows(driver.db(), inbound.get(), outbound.get()))
			return EXIT_FAILURE;
	}

	// load filter next
	boost::scoped_ptr<Cutter> cutter(new Cutter);
	if (with_filter) {
		if (!cutter->Load(filter_file.c_str(), layer_size)) return EXIT_FAILURE;
	}

	// load bc next
	int nol = 0;
	{
		boost::scoped_ptr<BcLoader> loader(new BcLoader(bc_file));
		if (!loader->Load(&nol, processor.get())) return EXIT_FAILURE;
	}
	if (nol != 1) { // nol should be always 1
		cerr << "invalid nol: " << nol << endl;
		return EXIT_FAILURE;
	}

	processor->CalculateCodeOffset();

	// replace nominal location in bytecode
	if (!processor->SolveLocation()) {
		return EXIT_FAILURE;
	}

	if (!processor->SolveDependencies(nol, inbound.get(), outbound.get(), false)) return EXIT_FAILURE;

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

	if (!processor->Process(executor.get())) return EXIT_FAILURE;

	if (!IsAllGreen(color.get(), layer_size)) {
		cerr << "could not calculate initial values of some variables:" << endl;
		layout->DetectRed(layer_size, color.get());
		return EXIT_FAILURE;
	}

	FILE *fp = fopen(output_file.c_str(), "wb");
	if (!fp) {
		std::perror(output_file.c_str());
		return EXIT_FAILURE;
	}
	if (with_filter) {
		if (!cutter->Apply(data.get(), fp)) {
			fclose(fp);
			return EXIT_FAILURE;
		}
	} else {
		// output the first layer of data
		if (fwrite(data.get(), sizeof(double), layer_size, fp) != layer_size) {
			cerr << "failed to write output: " << output_file.c_str() << endl;
			fclose(fp);
			return EXIT_FAILURE;
		}
	}
	fclose(fp);

	google::protobuf::ShutdownProtobufLibrary();

	return EXIT_SUCCESS;
}
