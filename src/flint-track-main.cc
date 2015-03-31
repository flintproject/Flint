/* -*- Mode: C++; tab-width: 4; indent-tabs-mode: t; c-basic-offset: 4 -*- vim:set ts=4 sw=4 sts=4 noet: */
#ifdef HAVE_CONFIG_H
#include "config.h"
#endif

#include <cstdio>
#include <cstdlib>
#include <cstring>
#include <iostream>
#include <memory>
#include <string>

#include <boost/noncopyable.hpp>
#include <boost/program_options.hpp>
#include <boost/scoped_ptr.hpp>
#include <boost/uuid/uuid_io.hpp>

#include "lo.pb.h"
#include "ipc.pb.h"

#include "bc/binary.h"
#include "filter/filter_loader.h"

namespace po = boost::program_options;

using std::cerr;
using std::endl;
using std::memcpy;
using std::string;

namespace {

class Filter : boost::noncopyable {
public:
	explicit Filter(ipc::SimulationTrack *st) : st_(st) {}

	void ReadHeader(int /*size*/) const {
		// ignore header
	}

	void ReadColumn(lo::Column *column) {
		if (column->position() == 0) {
			st_->add_key("time");
		} else {
			boost::uuids::uuid u;
			memcpy(&u, column->uuid().c_str(), 16);
			string us = to_string(u);
			st_->add_key(us+":"+column->name());
		}
		st_->add_name(column->name());
		st_->add_scope_name(column->track_name());
		st_->add_label(column->label());
		delete column;
	}

private:
	ipc::SimulationTrack *st_;
};

} // namespace

int main(int argc, char *argv[])
{
	GOOGLE_PROTOBUF_VERIFY_VERSION;

	RequestBinaryStdio();

	po::options_description opts("options");
	po::positional_options_description popts;
	po::variables_map vm;
	string filter_file;
	int print_help = 0;

	opts.add_options()
		("filter", po::value<string>(&filter_file), "Input filter file")
		("help,h", "Show this message");
	popts.add("filter", 1);

	try {
		po::store(po::command_line_parser(argc, argv).options(opts).positional(popts).run(), vm);
		po::notify(vm);
		if (vm.count("help") > 0) {
			print_help = 1;
		} else if ( vm.count("filter") == 0 ) {
			print_help = 2;
		}
	} catch (const po::error &) {
		print_help = 2;
	}
	if (print_help) {
		cerr << "usage: " << argv[0] << " FILTER" << endl;
		cerr << opts;
		return (print_help == 1) ? EXIT_SUCCESS : EXIT_FAILURE;
	}

	boost::scoped_ptr<ipc::SimulationTrack> st(new ipc::SimulationTrack);

	// load filter
	boost::scoped_ptr<Filter> filter(new Filter(st.get()));
	{
		boost::scoped_ptr<FilterLoader> loader(new FilterLoader(filter_file));
		if (!loader->Load(filter.get())) return EXIT_FAILURE;
	}

	if (!st->SerializeToOstream(&std::cout)) {
		cerr << "failed to serialize SimulationTrack" << endl;
		return EXIT_FAILURE;
	}

	google::protobuf::ShutdownProtobufLibrary();

	return EXIT_SUCCESS;
}
