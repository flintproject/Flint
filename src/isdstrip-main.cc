/* -*- Mode: C++; tab-width: 4; indent-tabs-mode: t; c-basic-offset: 4 -*- vim:set ts=4 sw=4 sts=4 noet: */
#ifdef HAVE_CONFIG_H
#include "config.h"
#endif

#include <cstdlib>
#include <cstring>
#include <iostream>
#include <iomanip>
#include <fstream>
#include <memory>
#include <string>
#include <vector>
#include <boost/program_options.hpp>

#include "isdstrip.h"
#include "sys/temporary_path.h"

namespace po = boost::program_options;

using namespace flint;

namespace {

void PrintNumOfColumns(std::uint32_t num_columns, const std::vector<std::uint32_t> &cv)
{
	std::cout << num_columns - static_cast<std::uint32_t>(cv.size()) << std::endl;
}

} // namespace

int main(int argc, char *argv[])
{
	po::options_description opts("options");
	po::positional_options_description popts;
	po::variables_map vm;
	std::string input_file, output_file;
	int print_help = 0;

	opts.add_options()
		("columns,c", "Put the resulting number of columns to stdout")
		("help,h", "Show this message")
		("output,o", po::value<std::string>(&output_file), "Output file name")
		("input", po::value<std::string>(&input_file), "Input file name");
	popts.add("input", 1);

	try {
		po::store(po::command_line_parser(argc, argv).options(opts).positional(popts).run(), vm);
		po::notify(vm);
		if (vm.count("help")) print_help = 1;
		if (vm.count("input") == 0) print_help = 2;
	} catch (const po::error &) {
		print_help = 2;
	}
	if (print_help != 0) {
		std::cerr << "usage: isdstrip [OPTIONS] PATH" << std::endl;
		std::cerr << opts << std::endl;
		return (print_help == 1) ? EXIT_SUCCESS : EXIT_FAILURE;
	}

	std::vector<std::uint32_t> cv;
	std::uint32_t num_columns = 0;
	const char *input_path = input_file.c_str();
	if (!isdstrip::ExtractConstantColumns(input_path, &num_columns, &cv)) {
		return EXIT_FAILURE;
	}
	if (vm.count("output")) {
		std::ofstream ofs(output_file.c_str(), std::ios::out|std::ios::binary);
		if (!ofs.is_open()) {
			std::cerr << "could not open output file: " << output_file << std::endl;
			return EXIT_FAILURE;
		}
		int r = isdstrip::Filter(input_path, cv, &ofs);
		ofs.close();
		if (r == EXIT_SUCCESS && vm.count("columns")) PrintNumOfColumns(num_columns, cv);
		return r;
	} else {
		char *output_path = nullptr;
		{
			std::unique_ptr<TemporaryPath> temp_path(new TemporaryPath("isdstrip"));
			output_path = temp_path->Touch();
			if (!output_path) {
				std::cerr << "could not create temporary path" << std::endl;
				return EXIT_FAILURE;
			}
		}
		std::ofstream ofs(output_path, std::ios::out|std::ios::binary);
		if (!ofs.is_open()) {
			std::cerr << "could not open output file: " << output_path << std::endl;
			remove(output_path);
			free(output_path);
			return EXIT_FAILURE;
		}
		int r = isdstrip::Filter(input_path, cv, &ofs);
		ofs.close();
		if (r == EXIT_SUCCESS) {
			// FIXME: thin possibility to fail to rename()
			remove(input_path);
			rename(output_path, input_path);
			if (vm.count("columns")) PrintNumOfColumns(num_columns, cv);
		} else {
			remove(output_path);
		}
		free(output_path);
		return r;
	}
}
