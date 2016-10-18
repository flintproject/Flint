/* -*- Mode: C++; tab-width: 4; indent-tabs-mode: t; c-basic-offset: 4 -*- vim:set ts=4 sw=4 sts=4 noet: */
#ifdef HAVE_CONFIG_H
#include "config.h"
#endif

#include "filter.h"

#include <cstdio>
#include <cstdlib>
#include <cstring>
#include <iostream>
#include <map>
#include <memory>

#include "lo.pb.h"

#include "filter/filter_loader.h"

namespace flint {
namespace filter {

namespace {

class Filter {
public:
	Filter(const Filter &) = delete;
	Filter &operator=(const Filter &) = delete;

	Filter() : size_(), columns_() {}

	void ReadHeader(int size) {
		size_ = size;
	}

	void ReadColumn(std::unique_ptr<lo::Column> &&column) {
		columns_.emplace(column->position(), column->col() * column->row());
	}

	bool Apply(const double *data, FILE *ofp) const {
		if (size_ == 0) {
			std::cerr << "size is zero" << std::endl;
			return false;
		}
		for (std::map<int, int>::const_iterator it=columns_.begin();it!=columns_.end();++it) {
			size_t p = static_cast<size_t>(it->first);
			size_t s = static_cast<size_t>(it->second);
			if (std::fwrite(data+p, sizeof(double), s, ofp) != s) {
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

}

bool Cut(const char *filter_file, const double *data, FILE *ofp)
{
	std::unique_ptr<Filter> filter(new Filter);
	{
		std::unique_ptr<FilterLoader> loader(new FilterLoader(filter_file));
		if (!loader->Load(filter.get())) return false;
	}
	return filter->Apply(data, ofp);
}

}
}
