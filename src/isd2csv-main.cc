/* -*- Mode: C++; tab-width: 4; indent-tabs-mode: t; c-basic-offset: 4 -*- vim:set ts=4 sw=4 sts=4 noet: */
#ifdef HAVE_CONFIG_H
#include "config.h"
#endif

#include <cstdlib>
#include <cstring>
#include <iostream>
#include <iomanip>
#include <fstream>
#include <string>
#include <vector>
#ifdef ENABLE_TCP
// We do not need Boost.Asio's threading support for isd2csv
#define BOOST_ASIO_DISABLE_THREADS
#include <boost/asio.hpp>
#endif
#include <boost/program_options.hpp>
#include "isdf/reader.h"

namespace po = boost::program_options;

using namespace flint;

namespace {

void RequestMaxNumOfDigits(std::ostream *os)
{
	// choose the defaultfloat
	os->unsetf(std::ios::floatfield);
	// See Theorem 15 of <http://docs.oracle.com/cd/E19957-01/806-3568/ncg_goldberg.html>.
	os->precision(17);
}

class Converter {
public:
#ifdef ENABLE_TCP
	Converter(std::ostream *os, std::uint32_t num_objs, size_t length = 0,
			  boost::asio::ip::udp::endpoint *endpoint = nullptr,
			  boost::asio::ip::udp::socket *socket = nullptr)
		: length_(length),
		  position_(),
		  endpoint_(endpoint),
		  socket_(socket),
		  progress_(-1),
		  os_(os),
		  num_objs_(num_objs),
		  descriptions_(num_objs),
		  units_(num_objs)
	{
		if (socket_) socket_->open(boost::asio::ip::udp::v4());
	}

	~Converter()
	{
		if (socket_ && socket_->is_open()) socket_->close();
	}
#else
	explicit Converter(std::ostream *os) : os_(os) {}
#endif

	void GetDescription(std::uint32_t i, size_t bytes, const char *desc) {
		descriptions_[i] = std::string(desc, bytes);
	}

	void GetUnit(std::uint32_t i, size_t bytes, const char *unit) {
		units_[i] = std::string(unit, bytes);
	}

	void WriteFirstLine() const {
		for (std::uint32_t i=0;i<num_objs_;i++) {
			if (i > 0) *os_ << ',';
			os_->write(descriptions_[i].c_str(), descriptions_[i].size());
			if (!units_[i].empty()) {
				*os_ << " (";
				os_->write(units_[i].c_str(), units_[i].size());
				*os_ << ")";
			}
		}
		*os_ << "\r\n";
	}

	int GetStep(size_t buf_size, char *buf) {
		char *eob = buf + buf_size;
		char *b = buf;
		while (b < eob) {
			double d;
			std::memcpy(&d, b, sizeof(double));
			if (b != buf) *os_ << ',';
			*os_ << d;
			b += sizeof(double);
		}
		*os_ << "\r\n";
#ifdef ENABLE_TCP
		if (endpoint_ && socket_) {
			position_ += buf_size;
			char p = static_cast<char>((position_ * 100) / length_);
			if (progress_ < p && 0 <= p && p <= 100) {
				progress_ = p;
				socket_->send_to(boost::asio::buffer(&progress_, 1), *endpoint_);
			}
		}
#endif
		return (b == eob) ? 1 : -1;
	}

private:
#ifdef ENABLE_TCP
	size_t length_, position_;
	boost::asio::ip::udp::endpoint *endpoint_;
	boost::asio::ip::udp::socket *socket_;
	char progress_;
#endif
	std::ostream *os_;
	std::uint32_t num_objs_;
	std::vector<std::string> descriptions_;
	std::vector<std::string> units_;
};

int Read(isdf::Reader &reader, Converter &converter, std::istream *is)
{
	if (!reader.SkipComment(is)) return EXIT_FAILURE;
	if (!reader.ReadDescriptions(converter, is)) return EXIT_FAILURE;
	if (!reader.ReadUnits(converter, is)) return EXIT_FAILURE;
	converter.WriteFirstLine();
	if (!reader.ReadSteps(converter, is)) return EXIT_FAILURE;
	return EXIT_SUCCESS;
}

#ifdef ENABLE_TCP
int Convert(const std::string &port, std::istream *is, std::ostream *os)
#else
int Convert(std::istream *is, std::ostream *os)
#endif
{
	isdf::Reader reader;
	if (!reader.ReadHeader(is)) return EXIT_FAILURE;
#ifdef ENABLE_TCP
	if (!port.empty()) {
		size_t p = is->tellg();
		is->seekg(0, std::ios::end);
		size_t q = is->tellg();
		if (q <= p) return EXIT_FAILURE;
		is->seekg(p, std::ios::beg);
		size_t length = q - p;

		boost::asio::io_service service;
		boost::asio::ip::udp::resolver resolver(service);
		boost::asio::ip::udp::resolver::query query(boost::asio::ip::udp::v4(), "127.0.0.1", port);
		boost::asio::ip::udp::endpoint endpoint(*resolver.resolve(query));
		boost::asio::ip::udp::socket socket(service);
		Converter converter(os, reader.num_objs(), length, &endpoint, &socket);
		return Read(reader, converter, is);
	} else {
		Converter converter(os, reader.num_objs());
		return Read(reader, converter, is);
	}
#else
	Converter converter(os);
	return Read(reader, converter, is);
#endif
}

} // namespace

int main(int argc, char *argv[])
{
	po::options_description opts("options");
	po::positional_options_description popts;
	po::variables_map vm;
	std::string input_file, output_file;
	int print_help = 0;
#ifdef ENABLE_TCP
	std::string port;
#endif

	opts.add_options()
		("help,h", "Show this message")
#ifdef ENABLE_TCP
		("progress", po::value<std::string>(&port), "Send progress in percentage")
#endif
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
	if (vm.count("output")) {
		std::ofstream ofs(output_file.c_str(), std::ios::out|std::ios::binary);
		if (!ofs.is_open()) {
			std::cerr << "could not open output file: " << output_file << std::endl;
			return EXIT_FAILURE;
		}
		if (vm.count("maximum-precision"))
			RequestMaxNumOfDigits(&ofs);
		if (vm.count("input")) {
			std::ifstream ifs(input_file.c_str(), std::ios::in|std::ios::binary);
			if (!ifs.is_open()) {
				std::cerr << "could not open input file: " << input_file << std::endl;
				return EXIT_FAILURE;
			}
#ifdef ENABLE_TCP
			r = Convert(port, &ifs, &ofs);
#else
			r = Convert(&ifs, &ofs);
#endif
			ifs.close();
		} else {
#ifdef ENABLE_TCP
			r = Convert(port, &std::cin, &ofs);
#else
			r = Convert(&std::cin, &ofs);
#endif
		}
		ofs.close();
	} else {
		if (vm.count("maximum-precision"))
			RequestMaxNumOfDigits(&std::cout);
		if (vm.count("input")) {
			std::ifstream ifs(input_file.c_str(), std::ios::in|std::ios::binary);
			if (!ifs.is_open()) {
				std::cerr << "could not open input file: " << input_file << std::endl;
				return EXIT_FAILURE;
			}
#ifdef ENABLE_TCP
			r = Convert(port, &ifs, &std::cout);
#else
			r = Convert(&ifs, &std::cout);
#endif
			ifs.close();
		} else {
#ifdef ENABLE_TCP
			r = Convert(port, &std::cin, &std::cout);
#else
			r = Convert(&std::cin, &std::cout);
#endif
		}
	}
	return r;
}
