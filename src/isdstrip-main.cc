/* -*- Mode: C++; tab-width: 4; indent-tabs-mode: t; c-basic-offset: 4 -*- vim:set ts=4 sw=4 sts=4 noet: */
#ifdef HAVE_CONFIG_H
#include "config.h"
#endif

#include <algorithm>
#include <cstdlib>
#include <cstring>
#include <iostream>
#include <iomanip>
#include <fstream>
#include <memory>
#include <string>
#include <sstream>
#include <vector>
#include <boost/program_options.hpp>
#include "isdf/isdf.h"
#include "isdf/reader.h"
#include "sys/temporary_path.h"

namespace po = boost::program_options;

using std::cerr;
using std::cout;
using std::endl;
using std::ifstream;
using std::ios;
using std::ofstream;
using std::ostream;
using std::string;
using std::vector;

namespace {

bool ExtractConstantColumns(const char *input,
							std::uint32_t *num_columns,
							vector<std::uint32_t> *cv)
{
	ifstream ifs(input, ios::in|ios::binary);
	if (!ifs.is_open()) {
		cerr << "could not open input file: " << input << endl;
		return false;
	}
	isdf::Reader reader;
	if ( !reader.ReadHeader(&ifs) ||
		 !reader.SkipComment(&ifs) ||
		 !reader.SkipDescriptions(&ifs) ||
		 !reader.SkipUnits(&ifs) ) {
		ifs.close();
		return false;
	}

	std::uint32_t num_objs = reader.num_objs();
	*num_columns = num_objs;

	size_t buf_size = num_objs * sizeof(double);
	std::unique_ptr<char[]> buf0(new char[buf_size]);
	ifs.read(buf0.get(), buf_size);
	if (ifs.fail()) {
		if (ifs.gcount() == 0) {
			ifs.close();
			// there seems no data, so let's keep all columns
			for (std::uint32_t i=0;i<num_objs;i++) {
				cv->push_back(i);
			}
			return true;
		} else {
			cerr << "failed to read data: " << input << endl;
		}
		ifs.close();
		return false;
	}

	for (std::uint32_t i=0;i<num_objs;i++) {
		cv->push_back(i);
	}
	std::unique_ptr<char[]> buf1(new char[buf_size]);
	for (;;) {
		ifs.read(buf1.get(), buf_size);
		if (ifs.fail()) {
			if (ifs.gcount() == 0) {
				// done
				break;
			} else {
				cerr << "failed to read data: " << input << endl;
				ifs.close();
				return false;
			}
		}
		vector<std::uint32_t>::iterator it = cv->begin();
		while (it != cv->end()) {
			size_t n = (*it) * sizeof(double);
			// we can ignore endianness by using memcmp()
			if (memcmp(buf0.get()+n, buf1.get()+n, sizeof(double)) == 0) {
				++it;
			} else {
				it = cv->erase(it);
			}
		}
		if (cv->empty()) break;
	}
	ifs.close();

	return true;
}

class StepFilter {
public:
	StepFilter(std::uint32_t num_objs, const vector<std::uint32_t> &cv, ostream *os)
		: num_objs_(num_objs),
		  cv_(cv),
		  os_(os),
		  buf_size_((num_objs-cv.size())*sizeof(double)),
		  buf_(new char[buf_size_])
	{}

	int GetStep(size_t, const char *buf) {
		const char *b0 = buf;
		char *b1 = buf_.get();
		vector<std::uint32_t>::const_iterator it = cv_.begin();
		for (std::uint32_t i=0;i<num_objs_;i++) {
			if (it != cv_.end() && i == *it) { // skip this entry
				++it;
			} else {
				memcpy(b1, b0, sizeof(double));
				b1 += sizeof(double);
			}
			b0 += sizeof(double);
		}
		os_->write(buf_.get(), buf_size_);
		if (!os_->good()) {
			cerr << "failed to write data" << endl;
			return -1;
		}
		return 1;
	}

private:
	std::uint32_t num_objs_;
	const vector<std::uint32_t> &cv_;
	ostream *os_;
	size_t buf_size_;
	std::unique_ptr<char[]> buf_;
};

int Filter(const char *input,
		   const vector<std::uint32_t> &cv,
		   ostream *os)
{
	ifstream ifs(input, ios::in|ios::binary);
	if (!ifs.is_open()) {
		cerr << "could not open file: " << input << endl;
		return EXIT_FAILURE;
	}
	isdf::Reader reader;
	if ( !reader.ReadHeader(&ifs) ||
		 !reader.SkipComment(&ifs) ||
		 !reader.ReadDescriptions(&ifs) ||
		 !reader.ReadUnits(&ifs) ) {
		ifs.close();
		return EXIT_FAILURE;
	}

	const char *rd = reader.descriptions();
	std::unique_ptr<char[]> descs(new char[reader.num_bytes_descs()]);
	char *d = descs.get();
	vector<std::uint32_t>::const_iterator it = cv.begin();
	for (std::uint32_t i=0;i<reader.num_objs();i++) {
		std::uint32_t bd = *reinterpret_cast<const std::uint32_t *>(rd);
		if (it != cv.end() && i == *it) { // skip this entry
			++it;
		} else {
			memcpy(d, rd, sizeof(bd) + bd);
			d += sizeof(bd) + bd;
		}
		rd += sizeof(bd) + bd;
	}

	const char *ru = reader.units();
	std::unique_ptr<char[]> units;
	char *u = NULL;
	if (ru) { // when units are available
		units.reset(new char[reader.num_bytes_units()]);
		u = units.get();
		it = cv.begin();
		for (std::uint32_t i=0;i<reader.num_objs();i++) {
			std::uint32_t bu = *reinterpret_cast<const std::uint32_t *>(ru);
			if (it != cv.end() && i == *it) { // skip this entry
				++it;
			} else {
				memcpy(u, ru, sizeof(bu) + bu);
				u += sizeof(bu) + bu;
			}
			ru += sizeof(bu) + bu;
		}
	}

	std::unique_ptr<char[]> header(new char[sizeof(isdf::ISDFHeader)]);
	memcpy(header.get(), &reader.header(), sizeof(isdf::ISDFHeader));
	reinterpret_cast<isdf::ISDFHeader *>(header.get())->num_objs -= cv.size();
	reinterpret_cast<isdf::ISDFHeader *>(header.get())->num_bytes_comment = 0; // discard the original comment
	reinterpret_cast<isdf::ISDFHeader *>(header.get())->num_bytes_descs = static_cast<std::uint32_t>(d - descs.get());
	if (ru) { // when units are available
		reinterpret_cast<isdf::ISDFHeader *>(header.get())->num_bytes_units = static_cast<std::uint32_t>(u - units.get());
	} else {
		reinterpret_cast<isdf::ISDFHeader *>(header.get())->num_bytes_units = 0;
	}

	os->write(header.get(), sizeof(isdf::ISDFHeader));
	if (!os->good()) {
		cerr << "could not write header" << endl;
		ifs.close();
		return EXIT_FAILURE;
	}
	os->write(descs.get(), d - descs.get());
	if (!os->good()) {
		cerr << "could not write descriptions" << endl;
		ifs.close();
		return EXIT_FAILURE;
	}
	if (ru) { // when units are available
		os->write(units.get(), u - units.get());
		if (!os->good()) {
			cerr << "could not write units" << endl;
			ifs.close();
			return EXIT_FAILURE;
		}
	}

	StepFilter sf(reader.num_objs(), cv, os);
	if (!reader.ReadSteps(sf, &ifs)) {
		ifs.close();
		return EXIT_FAILURE;
	}
	ifs.close();
	return EXIT_SUCCESS;
}

void PrintNumOfColumns(std::uint32_t num_columns, const vector<std::uint32_t> &cv)
{
	cout << num_columns - static_cast<std::uint32_t>(cv.size()) << endl;
}

} // namespace

int main(int argc, char *argv[])
{
	po::options_description opts("options");
	po::positional_options_description popts;
	po::variables_map vm;
	string input_file, output_file;
	int print_help = 0;

	opts.add_options()
		("columns,c", "Put the resulting number of columns to stdout")
		("help,h", "Show this message")
		("output,o", po::value<string>(&output_file), "Output file name")
		("input", po::value<string>(&input_file), "Input file name");
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
		cerr << "usage: isdstrip [OPTIONS] PATH" << endl;
		cerr << opts << endl;
		return (print_help == 1) ? EXIT_SUCCESS : EXIT_FAILURE;
	}

	std::unique_ptr<vector<std::uint32_t> > cv(new vector<std::uint32_t>);
	std::uint32_t num_columns = 0;
		const char *input_path = input_file.c_str();
		if (!ExtractConstantColumns(input_path, &num_columns, cv.get())) {
			return EXIT_FAILURE;
		}
		if (vm.count("output")) {
			ofstream ofs(output_file.c_str(), ios::out|ios::binary);
			if (!ofs.is_open()) {
				cerr << "could not open output file: " << output_file << endl;
				return EXIT_FAILURE;
			}
			int r = Filter(input_path, *cv, &ofs);
			ofs.close();
			if (r == EXIT_SUCCESS && vm.count("columns")) PrintNumOfColumns(num_columns, *cv);
			return r;
		} else {
			char *output_path = NULL;
			{
				std::unique_ptr<TemporaryPath> temp_path(new TemporaryPath("isdstrip"));
				output_path = temp_path->Touch();
				if (!output_path) {
					cerr << "could not create temporary path" << endl;
					return EXIT_FAILURE;
				}
			}
			ofstream ofs(output_path, ios::out|ios::binary);
			if (!ofs.is_open()) {
				cerr << "could not open output file: " << output_path << endl;
				remove(output_path);
				free(output_path);
				return EXIT_FAILURE;
			}
			int r = Filter(input_path, *cv, &ofs);
			ofs.close();
			if (r == EXIT_SUCCESS) {
				// FIXME: thin possibility to fail to rename()
				remove(input_path);
				rename(output_path, input_path);
				if (vm.count("columns")) PrintNumOfColumns(num_columns, *cv);
			} else {
				remove(output_path);
			}
			free(output_path);
			return r;
		}
}
