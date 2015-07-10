/* -*- Mode: C++; tab-width: 4; indent-tabs-mode: t; c-basic-offset: 4 -*- vim:set ts=4 sw=4 sts=4 noet: */
#include "copier.hh"

#include <cassert>
#include <cstdio>
#include <cstring>
#include <iostream>

using std::cerr;
using std::endl;

namespace layout {

namespace {

void Compact(const std::vector<int> &offsets, std::vector<std::pair<size_t, size_t> > *v)
{
	auto it = offsets.cbegin();
	while (it != offsets.cend()) {
		int o = *it;
		size_t s = sizeof(double);
		while (++it != offsets.cend()) {
			assert(o + 1 <= *it);
			if (o + 1 < *it)
				break;
			s += sizeof(double);
		}
		v->emplace_back(std::make_pair(size_t(o), s));
	}
}

}

Copier::Copier(const std::vector<int> &offsets)
	: v_()
{
	Compact(offsets, &v_);
}

void Copier::Copy(const double *source, double *target) const
{
	for (auto const &p : v_)
		std::memcpy(target + p.first, source + p.first, p.second);
}

}
