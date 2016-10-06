/* -*- Mode: C++; tab-width: 4; indent-tabs-mode: t; c-basic-offset: 4 -*- vim:set ts=4 sw=4 sts=4 noet: */
#ifndef FLINT_FILTER_FILTER_LOADER_H_
#define FLINT_FILTER_FILTER_LOADER_H_

#include <fstream>
#include <memory>
#include <string>

#include "bc/pack.h"

namespace flint {

class FilterLoader {
public:
	FilterLoader(const FilterLoader &) = delete;
	FilterLoader &operator=(const FilterLoader &) = delete;

	explicit FilterLoader(const std::string &file) : ifs_(file.c_str(), std::ios::in|std::ios::binary) {}

	~FilterLoader() {
		if (ifs_.is_open()) ifs_.close();
	}

	template<typename TFilter>
	bool Load(TFilter *filter) {
		if (!ifs_.is_open()) {
			std::cerr << "failed to open filter file" << std::endl;
			return false;
		}

		std::unique_ptr<lo::Header> header(new lo::Header);
		if (!UnpackFromIstream(*header, &ifs_)) {
			std::cerr << "could not read Header" << std::endl;
			return false;
		}
		filter->ReadHeader(header->size());
		while (ifs_.peek() != EOF) {
			std::unique_ptr<lo::Column> column(new lo::Column);
			if (!UnpackFromIstream(*column, &ifs_)) {
				std::cerr << "could not read Column" << std::endl;
				return false;
			}
			filter->ReadColumn(std::move(column));
		}
		return true;
	}

private:
	std::ifstream ifs_;
};

}

#endif
