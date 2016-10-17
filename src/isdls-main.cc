/* -*- Mode: C++; tab-width: 4; indent-tabs-mode: t; c-basic-offset: 4 -*- vim:set ts=4 sw=4 sts=4 noet: */
#include <cstdlib>
#include <iostream>
#include <fstream>
#include <string>
#include <boost/program_options.hpp>
#include "isdf/reader.h"

namespace po = boost::program_options;

using std::ifstream;
using std::istream;
using std::ios;
using std::memcpy;

using namespace flint;

namespace {

struct ColumnPrinter {
	void GetDescription(std::uint32_t, size_t bytes, const char *desc) {
		std::cout.write(desc, bytes);
		std::cout << std::endl;
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
	explicit IndexedColumnPrinter(std::uint32_t row) : row_(row), offset_(row * sizeof(double)) {}

	void GetDescription(std::uint32_t i, size_t bytes, const char *desc) {
		if (i == row_) {
			std::cout.write(desc, bytes);
			std::cout << std::endl;
		}
	}

	int GetStep(size_t, const char *buf) {
		double d;
		memcpy(&d, buf + offset_, sizeof(double));
		std::cout << d << std::endl;
		return 1;
	}

private:
	std::uint32_t row_;
	size_t offset_;
};

int ListRow(std::uint32_t row, istream *is)
{
	isdf::Reader reader;
	if (!reader.ReadHeader(is)) return EXIT_FAILURE;

	std::uint32_t num_objs = reader.num_objs();
	if (num_objs <= row) {
		std::cerr << "boundary error of index exceeding num_objs: " << num_objs << std::endl;
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
	std::cout << reader.GetDataOffset() << std::endl;
	return EXIT_SUCCESS;
}

} // namespace

int main(int argc, char *argv[])
{
	po::options_description opts("options");
	po::positional_options_description popts;
	po::variables_map vm;
	std::string input_file;
	std::uint32_t row = 0;
	int print_help = 0;

	opts.add_options()
		("columns,c", "Print the columns (default)")
		("data-offset", "Print the offset (in bytes) where the data section starts")
		("row,r", po::value<std::uint32_t>(&row), "Print the row of given index")
		("help,h", "Show this message")
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
		std::cerr << "usage: isdls [OPTIONS] [PATH]" << std::endl;
		std::cerr << opts;
		return (print_help == 1) ? EXIT_SUCCESS : EXIT_FAILURE;
	}

	int r;
	if (vm.count("input")) {
		ifstream ifs(input_file.c_str(), ios::in|ios::binary);
		if (!ifs.is_open()) {
			std::cerr << "could not open input file: " << input_file << std::endl;
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
			r = PrintDataOffset(&std::cin);
		} else if (vm.count("row")) {
			r = ListRow(row, &std::cin);
		} else {
			r = ListColumns(&std::cin);
		}
	}
	return r;
}
