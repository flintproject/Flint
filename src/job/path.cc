/* -*- Mode: C++; tab-width: 4; indent-tabs-mode: t; c-basic-offset: 4 -*- vim:set ts=4 sw=4 sts=4 noet: */
#ifdef HAVE_CONFIG_H
#include "config.h"
#endif

#include "job.h"

#include <cassert>
#include <cstdio>
#include <cstring>

namespace flint {
namespace job {

boost::filesystem::path BuildPath(const boost::filesystem::path &dir, int id)
{
	assert(id >= 0);

	char path[12];
	unsigned int u = static_cast<unsigned int>(id);
	unsigned int a, b, c, d;
	a = (u>>24)&0xFF;
	b = (u>>16)&0xFF;
	c = (u>> 8)&0xFF;
	d = (u    )&0xFF;
	std::sprintf(path, "%02x/%02x/%02x/%02x", a, b, c, d);
	return dir / path;
}

}
}
