/* -*- Mode: C++; tab-width: 4; indent-tabs-mode: t; c-basic-offset: 4 -*- vim:set ts=4 sw=4 sts=4 noet: */
#ifndef FLINT_FILTER_FILTER_LOADER_H_
#define FLINT_FILTER_FILTER_LOADER_H_

#include <fstream>
#include <memory>
#include <string>

#include "bc/pack.h"

namespace flint {

class FilterLoader : boost::noncopyable {
public:
	explicit FilterLoader(const std::string &file) : ifs_(file.c_str(), std::ios::in|std::ios::binary) {}

	~FilterLoader() {
		if (ifs_.is_open()) ifs_.close();
	}

	template<typename TFilter>
	bool Load(TFilter *filter) {
		using std::cerr;
		using std::endl;

		if (!ifs_.is_open()) {
			cerr << "failed to open filter file" << endl;
			return false;
		}

		std::unique_ptr<lo::Header> header(new lo::Header);
		if (!UnpackFromIstream(*header, &ifs_)) {
			cerr << "could not read Header" << endl;
			return false;
		}
		filter->ReadHeader(header->size());
		while (ifs_.peek() != EOF) {
			std::unique_ptr<lo::Column> column(new lo::Column);
			if (!UnpackFromIstream(*column, &ifs_)) {
				cerr << "could not read Column" << endl;
				return false;
			}
			filter->ReadColumn(column.release());
		}
		return true;
	}

private:
	std::ifstream ifs_;
};

}

#endif
