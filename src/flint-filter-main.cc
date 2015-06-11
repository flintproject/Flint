/* -*- Mode: C++; tab-width: 4; indent-tabs-mode: t; c-basic-offset: 4 -*- vim:set ts=4 sw=4 sts=4 noet: */
#ifdef HAVE_CONFIG_H
#include "config.h"
#endif

#include <cstdio>
#include <cstdlib>
#include <cstring>
#include <iostream>
#include <string>

#include <boost/program_options.hpp>

#include "lo.pb.h"

#include "db/read-only-driver.hh"
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
	string db_file, spec_file, layout_file, output_file;
	int print_help = 0;

	opts.add_options()
		("db", po::value<string>(&db_file), "Input database file")
		("spec", po::value<string>(&spec_file), "Input spec file")
		("layout", po::value<string>(&layout_file), "Input layout file")
		("output", po::value<string>(&output_file), "Output file")
		("help,h", "Show this message");
	popts.add("db", 1).add("spec", 1).add("layout", 1).add("output", 1);

	try {
		po::store(po::command_line_parser(argc, argv).options(opts).positional(popts).run(), vm);
		po::notify(vm);
		if (vm.count("help") > 0) {
			print_help = 1;
		} else if ( vm.count("db") == 0 ||
					vm.count("spec") == 0 ||
					vm.count("layout") == 0 ||
					vm.count("output") == 0 ) {
			print_help = 2;
		}
	} catch (const po::error &) {
		print_help = 2;
	}
	if (print_help) {
		cerr << "usage: " << argv[0] << " DB SPEC LAYOUT OUTPUT" << endl;
		cerr << opts;
		return (print_help == 1) ? EXIT_SUCCESS : EXIT_FAILURE;
	}

	db::ReadOnlyDriver driver(db_file.c_str());
	if (!filter::Create(driver.db(), spec_file.c_str(), layout_file.c_str(), output_file.c_str()))
		return EXIT_FAILURE;

	google::protobuf::ShutdownProtobufLibrary();

	return EXIT_SUCCESS;
}
