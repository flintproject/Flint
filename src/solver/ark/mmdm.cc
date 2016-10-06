/* -*- Mode: C++; tab-width: 4; indent-tabs-mode: t; c-basic-offset: 4 -*- vim:set ts=4 sw=4 sts=4 noet: */
#include "solver/ark/mmdm.h"

#ifdef HAVE_CONFIG_H
#include "config.h"
#endif

#include <cassert>
#include <cstdio>
#include <iostream>

namespace flint {
namespace solver {
namespace ark {

Mmdm::Mmdm(long size)
	: size_(size)
{
}

long Mmdm::size() const
{
	return size_;
}

void Mmdm::Add(long row, long col, long index)
{
	map_.emplace(std::make_pair(row, col), index);
}

long Mmdm::Find(long row, long col)
{
	auto it = map_.find(std::make_pair(row, col));
	if (it == map_.end())
		return 0;
	return it->second;
}

}
}
}
