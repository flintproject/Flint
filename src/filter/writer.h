/* -*- Mode: C++; tab-width: 4; indent-tabs-mode: t; c-basic-offset: 4 -*- vim:set ts=4 sw=4 sts=4 noet: */
#ifndef FLINT_FILTER_WRITER_H_
#define FLINT_FILTER_WRITER_H_

#include <cstdio>
#include <map>
#include <ostream>
#include <vector>

namespace flint {
namespace filter {

class Writer {
public:
	Writer(const Writer &) = delete;
	Writer &operator=(const Writer &) = delete;

	explicit Writer(const std::map<int, int> &m);

	bool Write(const double *data, FILE *fp) const;

	bool PrintCode(std::ostream *os) const;

private:
	std::vector<std::pair<size_t, size_t> > v_;
};

}
}

#endif
