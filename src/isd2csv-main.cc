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

using std::cerr;
using std::cin;
using std::cout;
using std::endl;
using std::ifstream;
using std::istream;
using std::ios;
using std::memcpy;
using std::ofstream;
using std::ostream;
using std::string;
using std::vector;

namespace {

class Converter {
public:
#ifdef ENABLE_TCP
	Converter(ostream *os, boost::uint32_t num_objs, size_t length = 0,
			  boost::asio::ip::udp::endpoint *endpoint = NULL,
			  boost::asio::ip::udp::socket *socket = NULL)
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
	explicit Converter(ostream *os) : os_(os) {}
#endif

	void GetDescription(boost::uint32_t i, size_t bytes, const char *desc) {
		descriptions_[i] = string(desc, bytes);
	}

	void GetUnit(boost::uint32_t i, size_t bytes, const char *unit) {
		units_[i] = string(unit, bytes);
	}

	void WriteFirstLine() const {
		for (boost::uint32_t i=0;i<num_objs_;i++) {
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
			memcpy(&d, b, sizeof(double));
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
	ostream *os_;
	boost::uint32_t num_objs_;
	vector<string> descriptions_;
	vector<string> units_;
};

int Read(isdf::Reader &reader, Converter &converter, istream *is)
{
	if (!reader.SkipComment(is)) return EXIT_FAILURE;
	if (!reader.ReadDescriptions(converter, is)) return EXIT_FAILURE;
	if (!reader.ReadUnits(converter, is)) return EXIT_FAILURE;
	converter.WriteFirstLine();
	if (!reader.ReadSteps(converter, is)) return EXIT_FAILURE;
	return EXIT_SUCCESS;
}

#ifdef ENABLE_TCP
int Convert(const std::string &port, istream *is, ostream *os)
#else
int Convert(istream *is, ostream *os)
#endif
{
	isdf::Reader reader;
	if (!reader.ReadHeader(is)) return EXIT_FAILURE;
#ifdef ENABLE_TCP
	if (!port.empty()) {
		size_t p = is->tellg();
		is->seekg(0, ios::end);
		size_t q = is->tellg();
		if (q <= p) return EXIT_FAILURE;
		is->seekg(p, ios::beg);
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
	string input_file, output_file;
	int print_help = 0;
#ifdef ENABLE_TCP
	string port;
#endif

	opts.add_options()
		("help,h", "Show this message")
#ifdef ENABLE_TCP
		("progress", po::value<string>(&port), "Send progress in percentage")
#endif
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
		cerr << "usage: isd2csv [OPTIONS] [PATH]" << endl;
		cerr << opts;
		return (print_help == 1) ? EXIT_SUCCESS : EXIT_FAILURE;
	}

	int r;
	if (vm.count("output")) {
		ofstream ofs(output_file.c_str(), ios::out|ios::binary);
		if (!ofs.is_open()) {
			cerr << "could not open output file: " << output_file << endl;
			return EXIT_FAILURE;
		}
		if (vm.count("input")) {
			ifstream ifs(input_file.c_str(), ios::in|ios::binary);
			if (!ifs.is_open()) {
				cerr << "could not open input file: " << input_file << endl;
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
			r = Convert(port, &cin, &ofs);
#else
			r = Convert(&cin, &ofs);
#endif
		}
		ofs.close();
	} else {
		if (vm.count("input")) {
			ifstream ifs(input_file.c_str(), ios::in|ios::binary);
			if (!ifs.is_open()) {
				cerr << "could not open input file: " << input_file << endl;
				return EXIT_FAILURE;
			}
#ifdef ENABLE_TCP
			r = Convert(port, &ifs, &cout);
#else
			r = Convert(&ifs, &cout);
#endif
			ifs.close();
		} else {
#ifdef ENABLE_TCP
			r = Convert(port, &cin, &cout);
#else
			r = Convert(&cin, &cout);
#endif
		}
	}
	return r;
}
