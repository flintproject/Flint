/* -*- Mode: C++; tab-width: 4; indent-tabs-mode: t; c-basic-offset: 4 -*- vim:set ts=4 sw=4 sts=4 noet: */
#ifdef HAVE_CONFIG_H
#include "config.h"
#endif

#include <cstdio>
#include <cstdlib>
#include <iostream>

#include <boost/program_options.hpp>

#include "phml/combine.h"

namespace po = boost::program_options;

using std::cerr;
using std::endl;
using std::string;

int main(int argc, char *argv[])
{
	po::options_description opts("options");
	po::positional_options_description popts;
	po::variables_map vm;
	string uuid, db_file, name_file, value_file, function_file, ode_file;
	int print_help = 0;

	opts.add_options()
		("uuid", po::value<string>(&uuid), "Input UUID")
		("db", po::value<string>(&db_file), "Input database file")
		("name", po::value<string>(&name_file), "Output name file")
		("value", po::value<string>(&value_file), "Output value file")
		("function", po::value<string>(&function_file), "Output function file")
		("ode", po::value<string>(&ode_file), "Output ODE file")
		("help,h", "Show this message");
	popts.add("uuid", 1).add("db", 1).add("name", 1).add("value", 1).add("function", 1).add("ode", 1);

	try {
		po::store(po::command_line_parser(argc, argv).options(opts).positional(popts).run(), vm);
		po::notify(vm);
		if (vm.count("help") > 0) {
			print_help = 1;
		} else if ( vm.count("uuid") == 0 ||
					vm.count("db") == 0 ||
					vm.count("name") == 0 ||
					vm.count("value") == 0 ||
					vm.count("function") == 0 ||
					vm.count("ode") == 0 ) {
			print_help = 2;
		}
	} catch (const po::error &) {
		print_help = 2;
	}
	if (print_help) {
		cerr << "usage: " << argv[0] << " UUID DB NAME VALUE FUNCTION ODE" << endl;
		cerr << opts;
		return (print_help == 1) ? EXIT_SUCCESS : EXIT_FAILURE;
	}

	if (!Combine(uuid.c_str(),
				 db_file.c_str(),
				 name_file.c_str(),
				 value_file.c_str(),
				 function_file.c_str(),
				 ode_file.c_str()))
		return EXIT_FAILURE;
	return EXIT_SUCCESS;
}
