/* -*- Mode: C++; tab-width: 4; indent-tabs-mode: t; c-basic-offset: 4 -*- vim:set ts=4 sw=4 sts=4 noet: */
#ifdef HAVE_CONFIG_H
#include "config.h"
#endif

#include "isd2csv.h"

#include <cstdio>
#include <cstdlib>
#include <cstring>
#include <iomanip>
#include <fstream>
#include <string>
#include <vector>
// To disable autolinking of Boost.Date_Time and Boost.Regex for MSVC
#define BOOST_DATE_TIME_NO_LIB
#define BOOST_REGEX_NO_LIB
// We do not need Boost.Asio's threading support for isd2csv
#define BOOST_ASIO_DISABLE_THREADS
#include <boost/asio.hpp>
#include <boost/uuid/string_generator.hpp>

#include "isdf/reader.h"

namespace flint {
namespace isd2csv {

namespace {

const size_t kPrefixLength = 36u;

bool ValidatePrefix(const std::string &name)
{
	boost::uuids::string_generator gen;
	try {
		gen(name.substr(0, kPrefixLength));
	} catch (const boost::exception &e) {
		return false;
	}
	return true;
}

class Converter {
public:
	Converter(const Option &option,
			  std::ostream *os, std::uint32_t num_objs, size_t length = 0,
			  boost::asio::ip::udp::endpoint *endpoint = nullptr,
			  boost::asio::ip::udp::socket *socket = nullptr)
		: option_(option)
		, length_(length)
		, position_()
		, endpoint_(endpoint)
		, socket_(socket)
		, progress_(-1)
		, os_(os)
		, num_objs_(num_objs)
		, descriptions_(num_objs)
		, units_(num_objs)
	{}

	~Converter()
	{
		if (socket_ && socket_->is_open()) {
			boost::system::error_code ec;
			socket_->shutdown(boost::asio::ip::udp::socket::shutdown_send, ec);
			socket_->close(ec);
		}
	}

	void GetDescription(std::uint32_t i, size_t bytes, const char *desc) {
		descriptions_[i] = std::string(desc, bytes);
	}

	void GetUnit(std::uint32_t i, size_t bytes, const char *unit) {
		units_[i] = std::string(unit, bytes);
	}

	void WriteFirstLine() const {
		size_t p;
		for (std::uint32_t i=0;i<num_objs_;i++) {
			if (i > 0) *os_ << ',';
			const std::string &name = descriptions_[i];
			const size_t s = name.size();
			if ( option_.ignore_prefixes &&
				 ( (p = name.find(':')) != std::string::npos) &&
				 p + 1 < s &&
				 p == kPrefixLength &&
				 ValidatePrefix(name) ) {
				os_->write(name.substr(p+1).c_str(), s-p-1);
			} else {
				os_->write(name.c_str(), s);
			}
			if (!option_.ignore_units && !units_[i].empty()) {
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
		if (endpoint_ && socket_ && socket_->is_open()) {
			position_ += buf_size;
			char p = static_cast<char>((position_ * 100) / length_);
			if (progress_ < p && 0 <= p && p <= 100) {
				progress_ = p;
				try {
					socket_->send_to(boost::asio::buffer(&progress_, 1), *endpoint_);
				} catch (boost::system::system_error &) {
					boost::system::error_code ec;
					socket_->shutdown(boost::asio::ip::udp::socket::shutdown_send, ec);
					socket_->close(ec);
				}
			}
		}
		return (b == eob) ? 1 : -1;
	}

private:
	const Option &option_;
	size_t length_, position_;
	boost::asio::ip::udp::endpoint *endpoint_;
	boost::asio::ip::udp::socket *socket_;
	char progress_;
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

}

int Convert(const Option &option, std::istream *is, std::ostream *os)
{
	isdf::Reader reader;
	if (!reader.ReadHeader(is)) return EXIT_FAILURE;
	if (!option.port.empty()) {
		size_t p = is->tellg();
		is->seekg(0, std::ios::end);
		size_t q = is->tellg();
		if (q <= p) return EXIT_FAILURE;
		is->seekg(p, std::ios::beg);
		size_t length = q - p;

		boost::asio::io_service service;
		boost::asio::ip::udp::resolver resolver(service);
		boost::asio::ip::udp::resolver::query query(boost::asio::ip::udp::v4(), "127.0.0.1", option.port);
		boost::system::error_code ec;
		auto it = resolver.resolve(query, ec);
		if (!ec) {
			boost::asio::ip::udp::endpoint endpoint(*it);
			boost::asio::ip::udp::socket socket(service);
			socket.open(boost::asio::ip::udp::v4(), ec);
			if (!ec) {
				Converter converter(option, os, reader.num_objs(), length, &endpoint, &socket);
				return Read(reader, converter, is);
			}
		}
	}
	Converter converter(option, os, reader.num_objs());
	return Read(reader, converter, is);
}

}
}
