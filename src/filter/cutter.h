/* -*- Mode: C++; tab-width: 4; indent-tabs-mode: t; c-basic-offset: 4 -*- vim:set ts=4 sw=4 sts=4 noet: */
#ifndef FLINT_FILTER_CUTTER_H_
#define FLINT_FILTER_CUTTER_H_

#include <cstdio>
#include <map>
#include <boost/noncopyable.hpp>
#include <boost/scoped_ptr.hpp>

#include "lo.pb.h"

#include "filter/filter_loader.h"

class Cutter : boost::noncopyable {
public:
	Cutter() : size_(), columns_() {}

	size_t size() const {return size_;}

	bool Load(const char *filter_file, size_t layer_size) {
		boost::scoped_ptr<FilterLoader> loader(new FilterLoader(filter_file));
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

	bool Apply(const double *data, FILE *fp) const {
		for (std::map<int, int>::const_iterator it=columns_.begin();it!=columns_.end();++it) {
			size_t p = static_cast<size_t>(it->first);
			size_t s = static_cast<size_t>(it->second);
			if (fwrite(data+p, sizeof(double), s, fp) != s) {
				std::cerr << "failed to filter output" << std::endl;
				return false;
			}
		}
		return true;
	}

private:
	size_t size_;
	std::map<int, int> columns_;
};

#endif
