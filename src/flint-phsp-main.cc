/* -*- Mode: C++; tab-width: 4; indent-tabs-mode: t; c-basic-offset: 4 -*- vim:set ts=4 sw=4 sts=4 noet: */
#include <cstdio>
#include <cstdlib>
#include <cstring>
#include <iostream>

#include "db/driver.h"
#include "phsp.hh"

using std::cerr;
using std::endl;

int main(int argc, char *argv[])
{
	using std::strcmp;

	if (argc != 2) {
		cerr << "usage: " << argv[0] << " DB" << endl;
		return EXIT_FAILURE;
	}
	if (strcmp(argv[1], "--help") == 0 || strcmp(argv[1], "-h") == 0) {
		cerr << "usage: " << argv[0] << " DB" << endl;
		return EXIT_SUCCESS;
	}

	db::Driver driver(argv[1]);
	return phsp::Read(driver.db()) ? EXIT_SUCCESS : EXIT_FAILURE;
}
