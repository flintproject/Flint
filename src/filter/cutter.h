/* -*- Mode: C++; tab-width: 4; indent-tabs-mode: t; c-basic-offset: 4 -*- vim:set ts=4 sw=4 sts=4 noet: */
#ifndef FLINT_FILTER_CUTTER_H_
#define FLINT_FILTER_CUTTER_H_

#include <cstdio>
#include <map>
#include <memory>
#include <boost/noncopyable.hpp>

#include "lo.pb.h"

#include "filter/filter_loader.h"
#include "filter/writer.hh"

namespace filter {

class Cutter : boost::noncopyable {
public:
	Cutter() : size_(), columns_() {}

	bool Load(const char *filter_file, size_t layer_size) {
		std::unique_ptr<FilterLoader> loader(new FilterLoader(filter_file));
		if (!loader->Load(this)) return false;
		if (size_ != layer_size) {
			std::cerr << "filter's size differs from layer's: " << size_ << " vs " << layer_size << std::endl;
			return false;
		}
		return true;
	}

	void ReadHeader(int size) {
		size_ = size;
	}

	void ReadColumn(lo::Column *column) {
		columns_.insert(std::make_pair(column->position(), column->size()));
		delete column;
	}

	Writer *CreateWriter() const {
		return new Writer(columns_);
	}

private:
	size_t size_;
	std::map<int, int> columns_;
};

}

#endif
