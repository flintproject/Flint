/* -*- Mode: C++; tab-width: 4; indent-tabs-mode: t; c-basic-offset: 4 -*- vim:set ts=4 sw=4 sts=4 noet: */
#ifdef HAVE_CONFIG_H
#include "config.h"
#endif

#include <cstdio>
#include <cstdlib>
#include <fstream>
#include <iomanip>
#include <iostream>
#include <string>

#include <boost/program_options.hpp>

#include "flint/numeric.h"
#include "isd2csv.h"

namespace po = boost::program_options;

using namespace flint;

int main(int argc, char *argv[])
{
	po::options_description opts("options");
	po::positional_options_description popts;
	po::variables_map vm;
	std::string input_file, output_file;
	int print_help = 0;
	isd2csv::Option option;

	opts.add_options()
		("help,h", "Show this message")
		("progress", po::value<std::string>(&option.port)->default_value(""), "Send progress in percentage")
		("ignore-prefixes,P", "Ignore variable prefixes")
		("ignore-units,U", "Ignore units")
		("maximum-precision,M", "Request the maximum number of decimal digits to print double-precision floating-point numbers")
		("output,o", po::value<std::string>(&output_file), "Output file name")
		("input", po::value<std::string>(&input_file), "Input file name");
	popts.add("input", 1);

	try {
		po::store(po::command_line_parser(argc, argv).options(opts).positional(popts).run(), vm);
		po::notify(vm);
		if (vm.count("help")) print_help = 1;
	} catch (const po::error &) {
		print_help = 2;
	}
	if (print_help != 0) {
		std::cerr << "usage: isd2csv [OPTIONS] [PATH]" << std::endl;
		std::cerr << opts;
		return (print_help == 1) ? EXIT_SUCCESS : EXIT_FAILURE;
	}

	int r;
	option.ignore_prefixes = (vm.count("ignore-prefixes") > 0);
	option.ignore_units = (vm.count("ignore-units") > 0);
	if (vm.count("output")) {
		std::ofstream ofs(output_file.c_str(), std::ios::out|std::ios::binary);
		if (!ofs.is_open()) {
			std::cerr << "could not open output file: " << output_file << std::endl;
			return EXIT_FAILURE;
		}
		if (vm.count("maximum-precision"))
			RequestMaxNumOfDigitsForDouble(ofs);
		if (vm.count("input")) {
			std::ifstream ifs(input_file.c_str(), std::ios::in|std::ios::binary);
			if (!ifs.is_open()) {
				std::cerr << "could not open input file: " << input_file << std::endl;
				return EXIT_FAILURE;
			}
			r = isd2csv::Convert(option, &ifs, &ofs);
			ifs.close();
		} else {
			r = isd2csv::Convert(option, &std::cin, &ofs);
		}
		ofs.close();
	} else {
		if (vm.count("maximum-precision"))
			RequestMaxNumOfDigitsForDouble(std::cout);
		if (vm.count("input")) {
			std::ifstream ifs(input_file.c_str(), std::ios::in|std::ios::binary);
			if (!ifs.is_open()) {
				std::cerr << "could not open input file: " << input_file << std::endl;
				return EXIT_FAILURE;
			}
			r = isd2csv::Convert(option, &ifs, &std::cout);
			ifs.close();
		} else {
			r = isd2csv::Convert(option, &std::cin, &std::cout);
		}
	}
	return r;
}
