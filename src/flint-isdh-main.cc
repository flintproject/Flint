/* -*- Mode: C++; tab-width: 4; indent-tabs-mode: t; c-basic-offset: 4 -*- vim:set ts=4 sw=4 sts=4 noet: */
#ifdef HAVE_CONFIG_H
#include "config.h"
#endif

#include <cassert>
#include <cstdlib>
#include <cstring>
#include <iostream>
#include <string>

#include <boost/program_options.hpp>

#include "lo.pb.h"

#include "filter.hh"

namespace po = boost::program_options;

using std::cerr;
using std::endl;
using std::string;

int main(int argc, char *argv[])
{
	GOOGLE_PROTOBUF_VERIFY_VERSION;

	po::options_description opts("options");
	po::positional_options_description popts;
	po::variables_map vm;
	string filter_file, output_file;
	int print_help = 0;

	opts.add_options()
		("filter", po::value<string>(&filter_file), "Filter file name")
		("output", po::value<string>(&output_file), "Output file")
		("help,h", "Show this message");
	popts.add("filter", 1).add("output", 1);

	try {
		po::store(po::command_line_parser(argc, argv).options(opts).positional(popts).run(), vm);
		po::notify(vm);
		if (vm.count("help") > 0) print_help = 1;
		if ( vm.count("filter") == 0 ||
			 vm.count("output") == 0 ) {
			print_help = 2;
		}
	} catch (const po::error &) {
		print_help = 2;
	}
	if (print_help) {
		cerr << "usage: " << argv[0] << " FILTER OUTPUT" << endl;
		cerr << opts;
		return (print_help == 1) ? EXIT_SUCCESS : EXIT_FAILURE;
	}

	if (!filter::Isdh(filter_file.c_str(), output_file.c_str()))
		return EXIT_FAILURE;

	google::protobuf::ShutdownProtobufLibrary();

	return EXIT_SUCCESS;
}
