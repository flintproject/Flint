/* -*- Mode: C++; tab-width: 4; indent-tabs-mode: t; c-basic-offset: 4 -*- vim:set ts=4 sw=4 sts=4 noet: */
#ifndef FLINT_FILTER_WRITER_HH_
#define FLINT_FILTER_WRITER_HH_

#include <cstdio>
#include <map>
#include <vector>

namespace flint {
namespace filter {

class Writer {
public:
	Writer(const Writer &) = delete;
	Writer &operator=(const Writer &) = delete;

	explicit Writer(const std::map<int, int> &m);

	bool Write(const double *data, FILE *fp) const;

private:
	std::vector<std::pair<size_t, size_t> > v_;
};

}
}

#endif
