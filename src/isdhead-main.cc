/* -*- Mode: C++; tab-width: 4; indent-tabs-mode: t; c-basic-offset: 4 -*- vim:set ts=4 sw=4 sts=4 noet: */
#ifdef HAVE_CONFIG_H
#include "config.h"
#endif

#include <cstdlib>
#include <cstring>
#include <iostream>
#include <fstream>
#include <memory>
#include <string>
#include <boost/noncopyable.hpp>
#include <boost/program_options.hpp>

#include "bc/binary.h"

#include "isdf/isdf.h"
#include "isdf/reader.h"

namespace po = boost::program_options;

using std::cerr;
using std::endl;
using std::string;

namespace {

class NumberOfRowsHandler : boost::noncopyable {
public:
	NumberOfRowsHandler(boost::uint32_t num_rows, std::ostream *os) : n_(), num_rows_(num_rows), os_(os) {}

	int GetStep(boost::uint32_t buf_size, const char *buf) {
		os_->write(buf, buf_size);
		if (!os_->good()) {
			cerr << "could not write step" << endl;
			return -1;
		}
		if (++n_ == num_rows_) {
			os_->flush();
			return 0;
		}
		return 1;
	}

private:
	boost::uint32_t n_;
	boost::uint32_t num_rows_;
	std::ostream *os_;
};

class EndOfTimeHandler : boost::noncopyable {
public:
	EndOfTimeHandler(double eot, std::ostream *os) : eot_(eot), os_(os) {}

	int GetStep(boost::uint32_t buf_size, const char *buf) {
		double t = 0;
		memcpy(&t, buf, sizeof(t));
		if (t < eot_) {
			os_->write(buf, buf_size);
			if (!os_->good()) {
				cerr << "could not write step" << endl;
				return -1;
			}
		} else {
			os_->flush();
			return 0;
		}
		return 1;
	}

private:
	double eot_;
	std::ostream *os_;
};

class Dam : boost::noncopyable {
public:
	explicit Dam(boost::uint32_t num_rows) : num_rows_(num_rows), eot_(), reader_() {}
	explicit Dam(double eot) : num_rows_(), eot_(eot), reader_() {}

	bool Pass(std::istream *is, std::ostream *os) {
		if (!reader_.ReadHeader(is)) return false;
		char harr[sizeof(isdf::ISDFHeader)];
		std::memcpy(harr, &reader_.header(), sizeof(isdf::ISDFHeader));
		os->write(harr, sizeof(isdf::ISDFHeader));
		if (!reader_.ReadComment(is)) return false;
		os->write(reader_.comment(), reader_.num_bytes_comment());
		if (!reader_.ReadDescriptions(is)) return false;
		os->write(reader_.descriptions(), reader_.num_bytes_descs());
		if (!reader_.ReadUnits(is)) return false;
		os->write(reader_.units(), reader_.num_bytes_units());
		if (num_rows_ > 0) {
			NumberOfRowsHandler handler(num_rows_, os);
			return reader_.ReadSteps(handler, is);
		} else {
			EndOfTimeHandler handler(eot_, os);
			return reader_.ReadSteps(handler, is);
		}
	}

private:
	boost::uint32_t num_rows_;
	double eot_;
	isdf::Reader reader_;
};

} // namespace

int main(int argc, char *argv[])
{
	RequestBinaryStdio();

	po::options_description opts("options");
	po::positional_options_description popts;
	po::variables_map vm;
	boost::uint32_t num_rows = 0;
	double eot = 0;
	string input_file, output_file;
	int print_help = 0;

	opts.add_options()
		("rows,n", po::value<boost::uint32_t>(&num_rows), "Specify the number of rows to be extracted")
		("eot,t", po::value<double>(&eot), "Specify the end of time")
		("help,h", "Show this message")
		("output,o", po::value<string>(&output_file), "Output file name")
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
		cerr << "usage: isdhead [OPTIONS] [PATH]" << endl;
		cerr << opts << endl;
		return (print_help == 1) ? EXIT_SUCCESS : EXIT_FAILURE;
	}

	std::unique_ptr<Dam> dam;
	if (vm.count("rows") > 0) {
		if (vm.count("eot") > 0) {
			cerr << "please specify one and only one of the following options: --rows(-n) / --eot(-t)" << endl;
			return EXIT_FAILURE;
		}
		dam.reset(new Dam(num_rows));
	} else if (vm.count("eot") > 0) {
		dam.reset(new Dam(eot));
	} else {
		cerr << "please specify one and only one of the following options: --rows(-n) / --eot(-t)" << endl;
		return EXIT_FAILURE;
	}
	if (vm.count("input")) {
		std::ifstream ifs(input_file.c_str(), std::ios::in|std::ios::binary);
		if (!ifs.is_open()) {
			cerr << "could not open file: " << input_file << endl;
			return EXIT_FAILURE;
		}
		if (vm.count("output")) {
			std::ofstream ofs(output_file.c_str(), std::ios::out|std::ios::binary);
			if (!ofs.is_open()) {
				cerr << "could not open output file: " << output_file << endl;
				return EXIT_FAILURE;
			}
			if (!dam->Pass(&ifs, &ofs)) return EXIT_FAILURE;
			ofs.close();
		} else {
			if (!dam->Pass(&ifs, &std::cout)) return EXIT_FAILURE;
		}
		ifs.close();
	} else if (vm.count("output")) {
		std::ofstream ofs(output_file.c_str(), std::ios::out|std::ios::binary);
		if (!ofs.is_open()) {
			cerr << "could not open output file: " << output_file << endl;
			return EXIT_FAILURE;
		}
		if (!dam->Pass(&std::cin, &ofs)) return EXIT_FAILURE;
		ofs.close();
	} else {
		if (!dam->Pass(&std::cin, &std::cout)) return EXIT_FAILURE;
	}

	return EXIT_SUCCESS;
}
