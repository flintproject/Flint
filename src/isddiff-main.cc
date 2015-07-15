/* -*- Mode: C++; tab-width: 4; indent-tabs-mode: t; c-basic-offset: 4 -*- vim:set ts=4 sw=4 sts=4 noet: */
#ifdef HAVE_CONFIG_H
#include "config.h"
#endif

#include <algorithm>
#include <cassert>
#include <cmath>
#include <cstdio>
#include <cstdlib>
#include <cstring>
#include <fstream>
#include <iostream>
#include <memory>
#include <string>

#include <boost/program_options.hpp>

#include "isdf/reader.h"

namespace po = boost::program_options;

using std::cerr;
using std::cout;
using std::endl;
using std::string;
using std::strncmp;

namespace {

const int kExitDifferent = 1;
const int kExitFailure = 2; // do not use EXIT_FAILURE, use this instead.

int CompareTimestamp(const isdf::ISDFHeader &header0,
					 const isdf::ISDFHeader &header1)
{
	if (strncmp(header0.timestamp, header1.timestamp, 20) != 0) {
		std::unique_ptr<char[]> ts0(new char[21]);
		std::unique_ptr<char[]> ts1(new char[21]);
		memcpy(ts0.get(), header0.timestamp, 20);
		memcpy(ts1.get(), header1.timestamp, 20);
		ts0[20] = 0;
		ts1[20] = 0;
		cout << "timestamp: "
			 << ts0.get()
			 << " vs "
			 << ts1.get()
			 << endl;
		return kExitDifferent;
	}
	return EXIT_SUCCESS;
}

int CompareComment(const isdf::Reader &reader0,
				   const isdf::Reader &reader1)
{
	boost::uint32_t nb0 = reader0.num_bytes_comment();
	boost::uint32_t nb1 = reader1.num_bytes_comment();
	if (nb0 != nb1) {
		cout << "comment" << endl;
		return kExitDifferent;
	}
	if (nb0 == 0) return EXIT_SUCCESS; // no comment
	if (strncmp(reader0.comment(), reader1.comment(), nb0) != 0) {
		cout << "comment" << endl;
		return kExitDifferent;
	}
	return EXIT_SUCCESS;
}

int CompareDescriptions(const isdf::Reader &reader0,
						const isdf::Reader &reader1)
{
	boost::uint32_t nb0 = reader0.num_bytes_descs();
	boost::uint32_t nb1 = reader1.num_bytes_descs();
	if (nb0 != nb1) {
		cout << "descriptions" << endl;
		return kExitDifferent;
	}
	if (strncmp(reader0.descriptions(), reader1.descriptions(), nb0) != 0) {
		cout << "descriptions" << endl;
		return kExitDifferent;
	}
	return EXIT_SUCCESS;
}

int CompareUnits(const isdf::Reader &reader0,
				 const isdf::Reader &reader1)
{
	boost::uint32_t nb0 = reader0.num_bytes_units();
	boost::uint32_t nb1 = reader1.num_bytes_units();
	if (nb0 != nb1) {
		cout << "units" << endl;
		return kExitDifferent;
	}
	if (nb0 == 0) return EXIT_SUCCESS; // no units
	if (strncmp(reader0.units(), reader1.units(), nb0) != 0) {
		cout << "units" << endl;
		return kExitDifferent;
	}
	return EXIT_SUCCESS;
}

void PrintDifference(boost::uint32_t i, boost::uint32_t k,
					 double v0, double v1)
{
	cout << "step " << i << " [" << k << "]: "
		 << v0
		 << " vs "
		 << v1
		 << endl;
}

int CompareBody(boost::uint32_t num_objs,
				double adelta, double rdelta,
				const string &input_file0, std::istream &is0,
				const string &input_file1, std::istream &is1)
{
	boost::uint32_t nb = num_objs * sizeof(double);
	assert(nb > 0);
	std::unique_ptr<char[]> buf0(new char[nb]);
	std::unique_ptr<char[]> buf1(new char[nb]);
	int r = EXIT_SUCCESS;
	for (boost::uint32_t i=0;;i++) {
		is0.read(buf0.get(), nb);
		is1.read(buf1.get(), nb);
		bool f0 = is0.fail();
		bool f1 = is1.fail();
		bool e0 = is0.eof();
		bool e1 = is1.eof();
		if (f0 != f1) {
			if (f0) {
				if (e0) {
					cout << "step " << i << " is missing in "
						 << input_file0
						 << endl;
					return kExitDifferent;
				}
			} else {
				if (e1) {
					cout << "step " << i << " is missing in "
						 << input_file1
						 << endl;
					return kExitDifferent;
				}
			}
		}
		if (f0) {
			if (e0 != e1) {
				cerr << "reading step " << i << " fails" << endl;
				return kExitDifferent; // TODO
			}
			return r;
		}
		if (is0.bad()) {
			cerr << "error occurred at reading "
				 << input_file0
				 << endl;
			return kExitFailure;
		}
		if (is1.bad()) {
			cerr << "error occurred at reading "
				 << input_file1
				 << endl;
			return kExitFailure;
		}

		if (strncmp(buf0.get(), buf1.get(), nb) == 0) continue;

		for (boost::uint32_t k=0;k<num_objs;k++) {
			double v0, v1;
			memcpy(&v0, &buf0[k * sizeof(double)], sizeof(double));
			memcpy(&v1, &buf1[k * sizeof(double)], sizeof(double));
			double av0 = std::fabs(v0);
			double av1 = std::fabs(v1);
			if (av0 < av1) std::swap(av0, av1);
			if ( (av0 - av1) > adelta &&
				 (1 - av1/av0) > rdelta ) {
				r = kExitDifferent;
				PrintDifference(i, k, v0, v1);
			}
		}
	}
}

} // namespace

int main(int argc, char *argv[])
{
	po::options_description opts("options");
	po::positional_options_description popts;
	po::variables_map vm;
	string input_file0, input_file1;
	double adelta = 0, rdelta = 0;
	int print_help = 0;

	opts.add_options()
		("absolute-delta,a", po::value<double>(&adelta), "tolerate given absolute delta")
		("relative-delta,r", po::value<double>(&rdelta), "tolerate given relative delta")
		("ignore-body,B", "ignore body")
		("ignore-comment,C", "ignore comment")
		("ignore-descriptions,D", "ignore descriptions")
		("ignore-timestamp,T", "ignore timestamp")
		("ignore-units,U", "ignore units")
		("help,h", "Show this message")
		("input0", po::value<string>(&input_file0), "the one of input file")
		("input1", po::value<string>(&input_file1), "the other input file");
	popts.add("input0", 1).add("input1", 1);

	try {
		po::store(po::command_line_parser(argc, argv).options(opts).positional(popts).run(), vm);
		po::notify(vm);
		if (vm.count("help")) print_help = 1;
		else if (vm.count("input0") == 0 || vm.count("input1") == 0) print_help = 2;
	} catch (const po::error &) {
		print_help = 2;
	}
	if (print_help != 0) {
		cerr << "usage: isddiff [OPTIONS] INPUT0 INPUT1" << endl;
		cerr << opts;
		return (print_help == 1) ? EXIT_SUCCESS : kExitFailure;
	}

	std::ifstream ifs0(input_file0.c_str(), std::ios::in|std::ios::binary);
	if (!ifs0.is_open()) {
		cerr << "failed to open file: " << input_file0 << endl;
		return kExitFailure;
	}

	std::ifstream ifs1(input_file1.c_str(), std::ios::in|std::ios::binary);
	if (!ifs1.is_open()) {
		ifs0.close();
		cerr << "failed to open file: " << input_file1 << endl;
		return kExitFailure;
	}

	std::unique_ptr<isdf::Reader> reader0(new isdf::Reader);
	if (!reader0->ReadHeader(&ifs0)) {
		ifs0.close();
		ifs1.close();
		return kExitFailure;
	}

	std::unique_ptr<isdf::Reader> reader1(new isdf::Reader);
	if (!reader1->ReadHeader(&ifs1)) {
		ifs0.close();
		ifs1.close();
		return kExitFailure;
	}

	int result = EXIT_SUCCESS;

	if (!vm.count("ignore-timestamp")) {
		int r = CompareTimestamp(reader0->header(), reader1->header());
		if (r != EXIT_SUCCESS) result = kExitDifferent;
	}

	boost::uint32_t no0 = reader0->num_objs();
	boost::uint32_t no1 = reader1->num_objs();
	if (no0 != no1) {
		result = kExitDifferent;
		cerr << "num_objs: "
			 << no0
			 << " vs "
			 << no1
			 << endl;
	}

	if (vm.count("ignore-comment")) {
		if ( !reader0->SkipComment(&ifs0) ||
			 !reader1->SkipComment(&ifs1) ) {
			ifs0.close();
			ifs1.close();
			return kExitFailure;
		}
	} else {
		if ( !reader0->ReadComment(&ifs0) ||
			 !reader1->ReadComment(&ifs1) ) {
			ifs0.close();
			ifs1.close();
			return kExitFailure;
		}
		int r = CompareComment(*reader0, *reader1);
		if (r != EXIT_SUCCESS) result = kExitDifferent;
	}

	if (vm.count("ignore-descriptions")) {
		if ( !reader0->SkipDescriptions(&ifs0) ||
			 !reader1->SkipDescriptions(&ifs1) ) {
			ifs0.close();
			ifs1.close();
			return kExitFailure;
		}
	} else {
		if ( !reader0->ReadDescriptions(&ifs0) ||
			 !reader1->ReadDescriptions(&ifs1) ) {
			ifs0.close();
			ifs1.close();
			return kExitFailure;
		}
		int r = CompareDescriptions(*reader0, *reader1);
		if (r != EXIT_SUCCESS) result = kExitDifferent;
	}

	if (vm.count("ignore-units")) {
		if ( !reader0->SkipUnits(&ifs0) ||
			 !reader1->SkipUnits(&ifs1) ) {
			ifs0.close();
			ifs1.close();
			return kExitFailure;
		}
	} else {
		if ( !reader0->ReadUnits(&ifs0) ||
			 !reader1->ReadUnits(&ifs1) ) {
			ifs0.close();
			ifs1.close();
			return kExitFailure;
		}
		int r = CompareUnits(*reader0, *reader1);
		if (r != EXIT_SUCCESS) result = kExitDifferent;
	}

	if (no0 != no1) return kExitDifferent;
	if (no0 == 0 || vm.count("ignore-body")) {
		ifs0.close();
		ifs1.close();
		return result;
	}

	int r = CompareBody(no0,
						adelta, rdelta,
						input_file0, ifs0,
						input_file1, ifs1);

	ifs0.close();
	ifs1.close();

	return (r == EXIT_SUCCESS) ? result : r;
}
