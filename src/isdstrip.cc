/* -*- Mode: C++; tab-width: 4; indent-tabs-mode: t; c-basic-offset: 4 -*- vim:set ts=4 sw=4 sts=4 noet: */
#ifdef HAVE_CONFIG_H
#include "config.h"
#endif

#include "isdstrip.h"

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
#include "isdf/isdf.h"
#include "isdf/reader.h"
#include "sys/temporary_path.h"

namespace flint {
namespace isdstrip {

bool ExtractConstantColumns(const char *input,
							std::uint32_t *num_columns,
							std::vector<std::uint32_t> *cv)
{
	std::ifstream ifs(input, std::ios::in|std::ios::binary);
	if (!ifs.is_open()) {
		std::cerr << "could not open input file: " << input << std::endl;
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
	if (num_columns)
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
			std::cerr << "failed to read data: " << input << std::endl;
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
				std::cerr << "failed to read data: " << input << std::endl;
				ifs.close();
				return false;
			}
		}
		std::vector<std::uint32_t>::iterator it = cv->begin();
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

namespace {

class StepFilter {
public:
	StepFilter(std::uint32_t num_objs, const std::vector<std::uint32_t> &cv, std::ostream *os)
		: num_objs_(num_objs),
		  cv_(cv),
		  os_(os),
		  buf_size_((num_objs-cv.size())*sizeof(double)),
		  buf_(new char[buf_size_])
	{}

	int GetStep(size_t, const char *buf) {
		const char *b0 = buf;
		char *b1 = buf_.get();
		std::vector<std::uint32_t>::const_iterator it = cv_.begin();
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
			std::cerr << "failed to write data" << std::endl;
			return -1;
		}
		return 1;
	}

private:
	std::uint32_t num_objs_;
	const std::vector<std::uint32_t> &cv_;
	std::ostream *os_;
	size_t buf_size_;
	std::unique_ptr<char[]> buf_;
};

}

int Filter(const char *input,
		   const std::vector<std::uint32_t> &cv,
		   std::ostream *os)
{
	std::ifstream ifs(input, std::ios::in|std::ios::binary);
	if (!ifs.is_open()) {
		std::cerr << "could not open file: " << input << std::endl;
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
	std::vector<std::uint32_t>::const_iterator it = cv.begin();
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
	char *u = nullptr;
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

	isdf::ISDFHeader header;
	memcpy(&header, &reader.header(), sizeof(header));
	header.num_objs -= cv.size();
	header.num_bytes_comment = 0; // discard the original comment
	header.num_bytes_descs = static_cast<std::uint32_t>(d - descs.get());
	if (ru) { // when units are available
		header.num_bytes_units = static_cast<std::uint32_t>(u - units.get());
	} else {
		header.num_bytes_units = 0;
	}

	os->write(reinterpret_cast<const char *>(&header), sizeof(header));
	if (!os->good()) {
		std::cerr << "could not write header" << std::endl;
		ifs.close();
		return EXIT_FAILURE;
	}
	os->write(descs.get(), d - descs.get());
	if (!os->good()) {
		std::cerr << "could not write descriptions" << std::endl;
		ifs.close();
		return EXIT_FAILURE;
	}
	if (ru) { // when units are available
		os->write(units.get(), u - units.get());
		if (!os->good()) {
			std::cerr << "could not write units" << std::endl;
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

}
}
