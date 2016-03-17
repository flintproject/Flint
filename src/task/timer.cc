/* -*- Mode: C++; tab-width: 4; indent-tabs-mode: t; c-basic-offset: 4 -*- vim:set ts=4 sw=4 sts=4 noet: */
#ifdef HAVE_CONFIG_H
#include "config.h"
#endif

#include "task.h"

#include <cassert>
#include <cstdio>
#include <cstdlib>
#include <iostream>

using std::cerr;
using std::endl;
using std::fwrite;

namespace flint {
namespace task {

bool Timer(double end, double dt, FILE *fp)
{
	double start = 0;
	if (fwrite(&start, sizeof(double), 1, fp) != 1) {
		cerr << "failed to write start" << endl;
		return false;
	}
	if (fwrite(&end, sizeof(double), 1, fp) != 1) {
		cerr << "failed to write end" << endl;
		return false;
	}
	if (fwrite(&dt, sizeof(double), 1, fp) != 1) {
		cerr << "failed to write dt" << endl;
		return false;
	}
	return true;
}

}
}
