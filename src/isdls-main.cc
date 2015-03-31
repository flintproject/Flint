/* -*- Mode: C++; tab-width: 4; indent-tabs-mode: t; c-basic-offset: 4 -*- vim:set ts=4 sw=4 sts=4 noet: */
#include <cstdlib>
#include <iostream>
#include <fstream>
#include <string>
#include <boost/program_options.hpp>
#include "isdf/reader.h"

namespace po = boost::program_options;

using std::cerr;
using std::cin;
using std::cout;
using std::endl;
using std::ifstream;
using std::istream;
using std::ios;
using std::string;

namespace {

struct ColumnPrinter {
	void GetDescription(boost::uint32_t, size_t bytes, const char *desc) {
		cout.write(desc, bytes);
		cout << endl;
	}
};

int ListColumns(istream *is)
{
	isdf::Reader reader;
	if (!reader.ReadHeader(is)) return EXIT_FAILURE;
	if (!reader.SkipComment(is)) return EXIT_FAILURE;
	ColumnPrinter printer;
	if (!reader.ReadDescriptions(printer, is)) return EXIT_FAILURE;
	return EXIT_SUCCESS;
}

class IndexedColumnPrinter {
public:
	IndexedColumnPrinter(boost::uint32_t row) : row_(row), offset_(row * sizeof(double)) {}

	void GetDescription(boost::uint32_t i, size_t bytes, const char *desc) {
		if (i == row_) {
			cout.write(desc, bytes);
			cout << endl;
		}
	}

	int GetStep(size_t, const char *buf) {
		double d = *reinterpret_cast<const double *>(buf + offset_);
		cout << d << endl;
		return 1;
	}

private:
	boost::uint32_t row_;
	size_t offset_;
};

int ListRow(boost::uint32_t row, istream *is)
{
	isdf::Reader reader;
	if (!reader.ReadHeader(is)) return EXIT_FAILURE;

	boost::uint32_t num_objs = reader.num_objs();
	if (num_objs <= row) {
		cerr << "boundary error of index exceeding num_objs: " << num_objs << endl;
		return EXIT_FAILURE;
	}

	if (!reader.SkipComment(is)) return EXIT_FAILURE;
	IndexedColumnPrinter printer(row);
	if (!reader.ReadDescriptions(printer, is)) return EXIT_FAILURE;
	if (!reader.SkipUnits(is)) return EXIT_FAILURE;
	if (!reader.ReadSteps(printer, is)) return EXIT_FAILURE;
	return EXIT_SUCCESS;
}

int PrintDataOffset(istream *is)
{
	isdf::Reader reader;
	if (!reader.ReadHeader(is)) return EXIT_FAILURE;
	cout << reader.GetDataOffset() << endl;
	return EXIT_SUCCESS;
}

} // namespace

int main(int argc, char *argv[])
{
	po::options_description opts("options");
	po::positional_options_description popts;
	po::variables_map vm;
	string input_file;
	boost::uint32_t row = 0;
	int print_help = 0;

	opts.add_options()
		("columns,c", "Print the columns (default)")
		("data-offset", "Print the offset (in bytes) where the data section starts")
		("row,r", po::value<boost::uint32_t>(&row), "Print the row of given index")
		("help,h", "Show this message")
		("input", po::value<string>(&input_file), "Input file name");
	popts.add("input", 1);

	try {
		po::store(po::command_line_parser(argc, argv).options(opts).positional(popts).run(), vm);
		po::notify(vm);
		if (vm.count("help")) print_help = 1;
	} catch (const po::error &) {
		print_help = 2;
	}
	if (print_help != 0) {
		cerr << "usage: isdls [OPTIONS] [PATH]" << endl;
		cerr << opts;
		return (print_help == 1) ? EXIT_SUCCESS : EXIT_FAILURE;
	}

	int r;
	if (vm.count("input")) {
		ifstream ifs(input_file.c_str(), ios::in|ios::binary);
		if (!ifs.is_open()) {
			cerr << "could not open input file: " << input_file << endl;
			return EXIT_FAILURE;
		}
		if (vm.count("data-offset")) {
			r = PrintDataOffset(&ifs);
		} else if (vm.count("row")) {
			r = ListRow(row, &ifs);
		} else {
			r = ListColumns(&ifs);
		}
		ifs.close();
	} else {
		if (vm.count("data-offset")) {
			r = PrintDataOffset(&cin);
		} else if (vm.count("row")) {
			r = ListRow(row, &cin);
		} else {
			r = ListColumns(&cin);
		}
	}
	return r;
}
