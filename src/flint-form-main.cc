/* -*- Mode: C++; tab-width: 4; indent-tabs-mode: t; c-basic-offset: 4 -*- vim:set ts=4 sw=4 sts=4 noet: */
#include <cstdio>
#include <cstdlib>
#include <cstring>
#include <iostream>

#include "db/driver.h"
#include "task.hh"

using std::cerr;
using std::endl;
using std::strcmp;

namespace {

void Usage()
{
	cerr << "usage: flint-form DB" << endl;
}

}

int main(int argc, char *argv[])
{
	if (argc < 2) {
		Usage();
		return EXIT_FAILURE;
	}
	if (strcmp(argv[1], "--help") == 0 || strcmp(argv[1], "-h") == 0) {
		Usage();
		return EXIT_SUCCESS;
	}

	db::Driver driver(argv[1]);
	return task::Form(driver.db()) ? EXIT_SUCCESS : EXIT_FAILURE;
}
