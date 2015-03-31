/* -*- Mode: C++; tab-width: 4; indent-tabs-mode: t; c-basic-offset: 4 -*- vim:set ts=4 sw=4 sts=4 noet: */
#ifdef HAVE_CONFIG_H
#include "config.h"
#endif

#include <cassert>
#include <cstdio>
#include <cstdlib>
#include <fstream>
#include <iostream>
#include <string>

#include <boost/program_options.hpp>
#include <boost/scoped_array.hpp>

#include "bc/binary.h"
#include "bc/index.h"

namespace po = boost::program_options;

using std::cerr;
using std::cin;
using std::endl;
using std::fwrite;
using std::string;

static const int kBufferSize = 1024;

namespace {

void CheckSize(size_t s)
{
	if (s < sizeof(double) * kOffsetBase) {
		cerr << "too short input" << endl;
		std::exit(EXIT_FAILURE);
	}
}

void SetTime(const po::variables_map &vm, double *ibuf, char *obuf)
{
	if (vm.count("time") > 0) {
		memcpy(obuf, ibuf, sizeof(double));
	}
	if (vm.count("end") > 0) {
		memcpy(obuf+sizeof(double), ibuf+1, sizeof(double));
	}
	if (vm.count("dt") > 0) {
		memcpy(obuf+sizeof(double)*2, ibuf+2, sizeof(double));
	}
}

} // namespace

int main(int argc, char *argv[])
{
	RequestBinaryStdio();

	po::options_description opts("options");
	po::positional_options_description popts;
	po::variables_map vm;
	double value[3];
	string input_file;
	int print_help = 0;

	opts.add_options()
		("time", po::value<double>(value), "time")
		("end", po::value<double>(value+1), "end")
		("dt", po::value<double>(value+2), "dt")
		("input", po::value<string>(&input_file), "Input file name")
		("help,h", "Show this message");
	popts.add("time", 1).add("end", 1).add("dt", 1).add("input", 1);

	try {
		po::store(po::command_line_parser(argc, argv).options(opts).positional(popts).run(), vm);
		po::notify(vm);
		if (vm.count("help") > 0) print_help = 1;
	} catch (const po::error &) {
		print_help = 2;
	}
	if (print_help) {
		cerr << "usage: " << argv[0] << " [OPTIONS]" << endl;
		cerr << opts;
		return (print_help == 1) ? EXIT_SUCCESS : EXIT_FAILURE;
	}

	if (vm.count("input") > 0) {
		std::ifstream ifs(input_file.c_str(), std::ios::in|std::ios::binary);
		if (!ifs.good()) {
			cerr << "could not open file: " << input_file << endl;
			return EXIT_FAILURE;
		}
		if (!ifs.seekg(0, ifs.end)) {
			cerr << "could not seek end of file: "  << input_file << endl;
			return EXIT_FAILURE;
		}
		size_t s = ifs.tellg();
		CheckSize(s);
		if (!ifs.seekg(0, ifs.beg)) {
			cerr << "could not seek beginning of file: " << input_file << endl;
		}
		boost::scoped_array<char> buf(new char[s]);
		if (!ifs.read(buf.get(), s)) {
			cerr << "could not read file: " << input_file << endl;
			return EXIT_FAILURE;
		}
		ifs.close();
		SetTime(vm, value, buf.get());
		if (fwrite(buf.get(), s, 1, stdout) != 1) {
			cerr << "failed to write output" << endl;
			return EXIT_FAILURE;
		}
	} else {
		boost::scoped_array<char> buf(new char[kBufferSize]);
		if (cin.read(buf.get(), kBufferSize).bad()) {
			cerr << "failed to read input" << endl;
			return EXIT_FAILURE;
		}
		size_t s = cin.gcount();
		CheckSize(s);
		SetTime(vm, value, buf.get());
		if (fwrite(buf.get(), s, 1, stdout) != 1) {
			cerr << "failed to write output" << endl;
			return EXIT_FAILURE;
		}
		while (cin.good()) {
			if (cin.read(buf.get(), kBufferSize).bad()) {
				cerr << "failed to read input" << endl;
				return EXIT_FAILURE;
			}
			s = cin.gcount();
			if (s == 0) continue;
			if (fwrite(buf.get(), s, 1, stdout) != 1) {
				cerr << "failed to write output" << endl;
				return EXIT_FAILURE;
			}
		}
	}

	return EXIT_SUCCESS;
}
