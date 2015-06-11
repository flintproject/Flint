/* -*- Mode: C++; tab-width: 4; indent-tabs-mode: t; c-basic-offset: 4 -*- vim:set ts=4 sw=4 sts=4 noet: */
#ifdef HAVE_CONFIG_H
#include "config.h"
#endif

#include <cassert>
#include <cstdio>
#include <cstdlib>
#include <cstring>
#include <iostream>

#include "db/driver.h"
#include "sbml.hh"

using std::cerr;
using std::endl;

namespace {

void Usage()
{
	cerr << "usage: flint-sbml DB" << endl;
}

} // namespace

int main(int argc, char *argv[])
{
	static const int kNumOfArgs = 2;

	if (argc != kNumOfArgs) {
		Usage();
		return EXIT_FAILURE;
	}
	if (strcmp("-h", argv[1]) == 0 || strcmp("--help", argv[1]) == 0) {
		Usage();
		return EXIT_SUCCESS;
	}

	db::Driver driver(argv[1]);
	return flint::sbml::Read(driver.db()) ? EXIT_SUCCESS : EXIT_FAILURE;
}
