/* -*- Mode: C++; tab-width: 4; indent-tabs-mode: t; c-basic-offset: 4 -*- vim:set ts=4 sw=4 sts=4 noet: */
#ifndef FLINT_ISDF_READER_H_
#define FLINT_ISDF_READER_H_

#include <cassert>
#include <cstring>
#include <iostream>
#include <memory>
#include "isdf/isdf.h"

namespace flint {
namespace isdf {

class Reader {
public:
	const ISDFHeader &header() const {return header_;}
	const char *comment() const {return comment_.get();}
	const char *descriptions() const {return descriptions_.get();}
	const char *units() const {return units_.get();}
	std::uint32_t num_bytes_comment() const {
		return header_.num_bytes_comment;
	}
	std::uint32_t num_bytes_descs() const {
		return header_.num_bytes_descs;
	}
	std::uint32_t num_bytes_units() const {
		return header_.num_bytes_units;
	}
	std::uint32_t num_objs() const {
		return header_.num_objs;
	}

	bool ReadHeader(std::istream *is) {
		char harr[sizeof(ISDFHeader)];
		is->read(harr, sizeof(ISDFHeader));
		if (!is->good()) {
			std::cerr << "could not read header" << std::endl;
			return false;
		}
		if (harr[0] != 'I' || harr[1] != 'S' ||
			harr[2] != 'D' || harr[3] != 'F') {
			std::cerr << "it seems unlike ISDF according to FourCC" << std::endl;
			return false;
		}
		std::memcpy(&header_, harr, sizeof(header_));
		if (header_.version != 1) {
			std::cerr << "unknown version of ISDF: " << header_.version << std::endl;
			return false;
		}
		return true;
	}

	size_t GetDataOffset() const {
		return sizeof(ISDFHeader) + num_bytes_comment() + num_bytes_descs() + num_bytes_units();
	}

	bool ReadComment(std::istream *is) {
		std::uint32_t n = num_bytes_comment();
		if (n == 0) return true;
		comment_.reset(new char[n]);
		is->read(comment_.get(), n);
		if (!is->good()) {
			std::cerr << "could not read comment" << std::endl;
			return false;
		}
		return true;
	}

	bool SkipComment(std::istream *is) {
		std::uint32_t n = num_bytes_comment();
		if (n == 0) return true;
		is->seekg(n, std::ios::cur);
		if (!is->good()) {
			std::cerr << "could not reach end of comment" << std::endl;
			return false;
		}
		return true;
	}

	bool ReadDescriptions(std::istream *is) {
		descriptions_.reset(new char[num_bytes_descs()]);
		is->read(descriptions_.get(), num_bytes_descs());
		if (!is->good()) {
			std::cerr << "could not read descriptions" << std::endl;
			return false;
		}
		return true;
	}

	template<typename TDescHandler>
	bool ReadDescriptions(TDescHandler &handler, std::istream *is) {
		if (!ReadDescriptions(is)) return false;

		const char *d = descriptions_.get();
		for (std::uint32_t i=0;i<num_objs();i++) {
#ifdef TYPE_PUNNING_BY_CAST
			std::uint32_t bytes = *reinterpret_cast<const std::uint32_t *>(d);
#else
			std::uint32_t bytes = 0;
			std::memcpy(&bytes, d, sizeof(bytes));
#endif
			if (d + bytes > descriptions_.get() + num_bytes_descs()) {
				std::cerr << "exceeded end of descriptions: " << bytes << std::endl;
				return false;
			}
			d += sizeof(std::uint32_t);
			handler.GetDescription(i, bytes, d);
			d += bytes;
		}
		if (d != descriptions_.get() + num_bytes_descs()) {
			std::cerr << "wrong value of num_bytes_descs: " << num_bytes_descs() << std::endl;
			return false;
		}
		return true;
	}

	bool SkipDescriptions(std::istream *is) {
		is->seekg(num_bytes_descs(), std::ios::cur);
		if (!is->good()) {
			std::cerr << "could not reach end of descriptions" << std::endl;
			return false;
		}
		return true;
	}

	bool ReadUnits(std::istream *is) {
		std::uint32_t n = num_bytes_units();
		if (n == 0) return true;
		units_.reset(new char[n]);
		is->read(units_.get(), n);
		if (!is->good()) {
			std::cerr << "could not read units" << std::endl;
			return false;
		}
		return true;
	}

	template<typename THandler>
	bool ReadUnits(THandler &handler, std::istream *is) {
		std::uint32_t n = num_bytes_units();
		if (n == 0) return true;
		units_.reset(new char[n]);
		is->read(units_.get(), n);
		if (!is->good()) {
			std::cerr << "could not read units" << std::endl;
			return false;
		}

		const char *d = units_.get();
		for (std::uint32_t i=0;i<num_objs();i++) {
#ifdef TYPE_PUNNING_BY_CAST
			std::uint32_t bytes = *reinterpret_cast<const std::uint32_t *>(d);
#else
			std::uint32_t bytes = 0;
			std::memcpy(&bytes, d, sizeof(bytes));
#endif
			if (d + bytes > units_.get() + num_bytes_units()) {
				std::cerr << "exceeded end of units: " << bytes << std::endl;
				return false;
			}
			d += sizeof(std::uint32_t);
			handler.GetUnit(i, bytes, d);
			d += bytes;
		}
		if (d != units_.get() + num_bytes_units()) {
			std::cerr << "wrong value of num_bytes_units: " << num_bytes_units() << std::endl;
			return false;
		}
		return true;
	}

	bool SkipUnits(std::istream *is) {
		std::uint32_t n = num_bytes_units();
		if (n == 0) return true;
		is->seekg(n, std::ios::cur);
		if (!is->good()) {
			std::cerr << "could not reach end of units" << std::endl;
			return false;
		}
		return true;
	}

	template<typename TStepHandler>
	bool ReadSteps(TStepHandler &handler, std::istream *is) {
		size_t buf_size = num_objs() * sizeof(double);
		if (buf_size == 0) {
			std::cerr << "num_objs was 0" << std::endl;
			return false;
		}
		std::unique_ptr<char[]> buf(new char[buf_size]);
		bool r = false;
		for (;;) {
			is->read(buf.get(), buf_size);
			if (is->fail()) {
				if (is->gcount() == 0) {
					r = true;
					break;
				} else {
					std::cerr << "truncated input" << std::endl;
					break;
				}
			}
			if (is->bad()) {
				std::cerr << "error occurred at reading input" << std::endl;
				break;
			}
			int i = handler.GetStep(buf_size, buf.get());
			switch (i) {
			case 0: // exit successfully
				return true;
			case 1: // go to next step
				break;
			default: // exit abnormally
				return false;
			}
		}
		return r;
	}

	bool CountSteps(std::istream *is, size_t *num_steps) const {
		size_t buf_size = num_objs() * sizeof(double);
		if (buf_size == 0) {
			std::cerr << "num_objs was 0" << std::endl;
			return false;
		}
		auto p = is->tellg();
		if (p < 0) {
			std::cerr << "failed to get position in input stream" << std::endl;
			return false;
		}
		is->seekg(0, std::ios::end);
		if (is->fail()) {
			std::cerr << "truncated input" << std::endl;
			return false;
		}
		if (is->bad()) {
			std::cerr << "error occurred at ignoring input" << std::endl;
			return false;
		}
		auto q = is->tellg();
		if (q < 0) {
			std::cerr << "failed to get position in input stream" << std::endl;
			return false;
		}
		assert(p <= q);
		size_t len = q - p;
		if (len % buf_size != 0) {
			std::cerr << "truncated input: " << len << "/" << buf_size << std::endl;
			return false;
		}
		*num_steps = len / buf_size;
		return true;
	}

private:
	ISDFHeader header_;
	std::unique_ptr<char[]> comment_;
	std::unique_ptr<char[]> descriptions_;
	std::unique_ptr<char[]> units_;
};

}
}

#endif // ISDF_READER_H_
