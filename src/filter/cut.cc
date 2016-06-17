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

using std::cerr;
using std::endl;
using std::make_pair;
using std::map;

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
		columns_.insert(make_pair(column->position(), column->col() * column->row()));
	}

	bool Apply(FILE *ifp, FILE *ofp) const {
		if (size_ == 0) {
			cerr << "size is zero" << endl;
			return false;
		}
		std::unique_ptr<double[]> data(new double[size_]);
		for (;;) {
			size_t s = fread(data.get(), sizeof(double), size_, ifp);
			if (s == 0) break;
			if (s != size_) {
				cerr << "too short input" << endl;
				return false;
			}
			for (map<int, int>::const_iterator it=columns_.begin();it!=columns_.end();++it) {
				size_t p = static_cast<size_t>(it->first);
				s = static_cast<size_t>(it->second);
				if (fwrite(data.get()+p, sizeof(double), s, ofp) != s) {
					cerr << "failed to filter output" << endl;
					return false;
				}
			}
		}
		return true;
	}

private:
	size_t size_;
	map<int, int> columns_;
};

}

bool Cut(const char *filter_file, FILE *ifp, FILE *ofp)
{
	std::unique_ptr<Filter> filter(new Filter);
	{
		std::unique_ptr<FilterLoader> loader(new FilterLoader(filter_file));
		if (!loader->Load(filter.get())) return false;
	}
	return filter->Apply(ifp, ofp);
}

}
}
