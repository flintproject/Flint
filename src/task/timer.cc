/* -*- Mode: C++; tab-width: 4; indent-tabs-mode: t; c-basic-offset: 4 -*- vim:set ts=4 sw=4 sts=4 noet: */
#ifdef HAVE_CONFIG_H
#include "config.h"
#endif

#include "task.h"

#include "bc/index.h"

namespace flint {
namespace task {

bool Timer(double end, double dt, double *data)
{
	data[kIndexTime] = 0;
	data[kIndexEnd] = end;
	data[kIndexDt] = dt;
	return true;
}

}
}
