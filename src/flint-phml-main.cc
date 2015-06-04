/* -*- Mode: C++; tab-width: 4; indent-tabs-mode: t; c-basic-offset: 4 -*- vim:set ts=4 sw=4 sts=4 noet: */
#include "phml.hh"

#include <cstdio>
#include <cstdlib>
#include <cstring>
#include <iostream>

using std::cerr;
using std::endl;
using std::strcmp;

#include "db/driver.h"

namespace {

void Usage()
{
	cerr << "usage: flint-phml DB" << endl;
}

}

int main(int argc, char *argv[])
{
	if (argc != 2) {
		Usage();
		return EXIT_FAILURE;
	}
	if (strcmp(argv[1], "--help") == 0 || strcmp(argv[1], "-h") == 0) {
		Usage();
		return EXIT_SUCCESS;
	}

	db::Driver driver(argv[1]);
	return phml::Read(driver.db()) ? EXIT_SUCCESS : EXIT_FAILURE;
}
