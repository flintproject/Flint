/* -*- Mode: C++; tab-width: 4; indent-tabs-mode: t; c-basic-offset: 4 -*- vim:set ts=4 sw=4 sts=4 noet: */
#ifdef HAVE_CONFIG_H
#include "config.h"
#endif

#include <cstdio>
#include <cstdlib>
#include <cstring>
#include <fstream>
#include <iostream>
#include <string>

#include <boost/program_options.hpp>

#include "lo.pb.h"

#include "db/read-only-driver.hh"
#include "job.hh"

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
	string db_file, source_layout_file, source_data_file, target_layout_file, target_data_file;
	int print_help = 0;

	opts.add_options()
		("db", po::value<string>(&db_file), "Input database file")
		("source_layout", po::value<string>(&source_layout_file), "Source layout file")
		("source_data", po::value<string>(&source_data_file), "Source data file")
		("target_layout", po::value<string>(&target_layout_file), "Output layout file")
		("target_data", po::value<string>(&target_data_file), "Output data file")
		("help,h", "Show this message");
	popts.add("db", 1).add("source_layout", 1).add("source_data", 1).add("target_layout", 1).add("target_data", 1);

	try {
		po::store(po::command_line_parser(argc, argv).options(opts).positional(popts).run(), vm);
		po::notify(vm);
		if (vm.count("help") > 0) {
			print_help = 1;
		}
		if (vm.count("db") == 0 ||
			vm.count("source_layout") == 0 ||
			vm.count("source_data") == 0 ||
			vm.count("target_layout") == 0 ||
			vm.count("target_data") == 0) {
			print_help = 2;
		}
	} catch (const po::error &) {
		print_help = 2;
	}
	if (print_help) {
		cerr << "usage: " << argv[0] << " DB SOURCE_LAYOUT SOURCE_DATA TARGET_LAYOUT TARGET_DATA" << endl;
		cerr << opts;
		return (print_help == 1) ? EXIT_SUCCESS : EXIT_FAILURE;
	}

	db::ReadOnlyDriver driver(db_file.c_str());
	if (!job::Store(driver.db(),
					source_layout_file.c_str(), source_data_file.c_str(),
					target_layout_file.c_str(), target_data_file.c_str()))
		return EXIT_FAILURE;

	google::protobuf::ShutdownProtobufLibrary();

	return EXIT_SUCCESS;
}
