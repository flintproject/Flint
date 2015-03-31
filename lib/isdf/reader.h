/* -*- Mode: C++; tab-width: 4; indent-tabs-mode: t; c-basic-offset: 4 -*- vim:set ts=4 sw=4 sts=4 noet: */
#ifndef ISDF_READER_H_
#define ISDF_READER_H_

#include <cstring>
#include <iostream>
#include <boost/scoped_array.hpp>
#include "isdf/isdf.h"

namespace isdf {

class Reader {
public:
	Reader() : header_(), comment_(NULL), descriptions_(NULL), units_(NULL) {}
	~Reader() {
		delete [] units_;
		delete [] descriptions_;
		delete [] comment_;
	}

	const ISDFHeader &header() const {return header_;}
	const char *comment() const {return comment_;}
	const char *descriptions() const {return descriptions_;}
	const char *units() const {return units_;}
	boost::uint32_t num_bytes_comment() const {
		return header_.num_bytes_comment;
	}
	boost::uint32_t num_bytes_descs() const {
		return header_.num_bytes_descs;
	}
	boost::uint32_t num_bytes_units() const {
		return header_.num_bytes_units;
	}
	boost::uint32_t num_objs() const {
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
		boost::uint32_t n = num_bytes_comment();
		if (n == 0) return true;
		comment_ = new char[n];
		is->read(comment_, n);
		if (!is->good()) {
			std::cerr << "could not read comment" << std::endl;
			return false;
		}
		return true;
	}

	bool SkipComment(std::istream *is) {
		boost::uint32_t n = num_bytes_comment();
		if (n == 0) return true;
		is->seekg(n, std::ios::cur);
		if (!is->good()) {
			std::cerr << "could not reach end of comment" << std::endl;
			return false;
		}
		return true;
	}

	bool ReadDescriptions(std::istream *is) {
		descriptions_ = new char[num_bytes_descs()];
		is->read(descriptions_, num_bytes_descs());
		if (!is->good()) {
			std::cerr << "could not read descriptions" << std::endl;
			return false;
		}
		return true;
	}

	template<typename TDescHandler>
	bool ReadDescriptions(TDescHandler &handler, std::istream *is) {
		if (!ReadDescriptions(is)) return false;

		const char *d = descriptions_;
		for (boost::uint32_t i=0;i<num_objs();i++) {
#ifdef TYPE_PUNNING_BY_CAST
			boost::uint32_t bytes = *reinterpret_cast<const boost::uint32_t *>(d);
#else
			boost::uint32_t bytes = 0;
			std::memcpy(&bytes, d, sizeof(bytes));
#endif
			if (d + bytes > descriptions_ + num_bytes_descs()) {
				std::cerr << "exceeded end of descriptions: " << bytes << std::endl;
				return false;
			}
			d += sizeof(boost::uint32_t);
			handler.GetDescription(i, bytes, d);
			d += bytes;
		}
		if (d != descriptions_ + num_bytes_descs()) {
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
		boost::uint32_t n = num_bytes_units();
		if (n == 0) return true;
		units_ = new char[n];
		is->read(units_, n);
		if (!is->good()) {
			std::cerr << "could not read units" << std::endl;
			return false;
		}
		return true;
	}

	template<typename THandler>
	bool ReadUnits(THandler &handler, std::istream *is) {
		boost::uint32_t n = num_bytes_units();
		if (n == 0) return true;
		units_ = new char[n];
		is->read(units_, n);
		if (!is->good()) {
			std::cerr << "could not read units" << std::endl;
			return false;
		}

		const char *d = units_;
		for (boost::uint32_t i=0;i<num_objs();i++) {
#ifdef TYPE_PUNNING_BY_CAST
			boost::uint32_t bytes = *reinterpret_cast<const boost::uint32_t *>(d);
#else
			boost::uint32_t bytes = 0;
			std::memcpy(&bytes, d, sizeof(bytes));
#endif
			if (d + bytes > units_ + num_bytes_units()) {
				std::cerr << "exceeded end of units: " << bytes << std::endl;
				return false;
			}
			d += sizeof(boost::uint32_t);
			handler.GetUnit(i, bytes, d);
			d += bytes;
		}
		if (d != units_ + num_bytes_units()) {
			std::cerr << "wrong value of num_bytes_units: " << num_bytes_units() << std::endl;
			return false;
		}
		return true;
	}

	bool SkipUnits(std::istream *is) {
		boost::uint32_t n = num_bytes_units();
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
		boost::scoped_array<char> buf(new char[buf_size]);
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

private:
	ISDFHeader header_;
	char *comment_;
	char *descriptions_;
	char *units_;
};

} // namespace isdf

#endif // ISDF_READER_H_
