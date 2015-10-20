/* -*- Mode: C++; tab-width: 4; indent-tabs-mode: t; c-basic-offset: 4 -*- vim:set ts=4 sw=4 sts=4 noet: */
#ifdef HAVE_CONFIG_H
#include "config.h"
#endif

#include "job.hh"

#include <cassert>
#include <cstdio>
#include <cstring>

using std::sprintf;
using std::strlen;

namespace flint {
namespace job {

char *BuildPath(const char *dir, int id)
{
	assert(dir);
	assert(id >= 0);

	size_t len = strlen(dir);
	char *path = new char[len+13];
	unsigned int u = static_cast<unsigned int>(id);
	unsigned int a, b, c, d;
	a = (u>>24)&0xFF;
	b = (u>>16)&0xFF;
	c = (u>> 8)&0xFF;
	d = (u    )&0xFF;
	sprintf(path, "%s/%02x/%02x/%02x/%02x", dir, a, b, c, d);
	return path;
}

}
}
