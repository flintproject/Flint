/* -*- Mode: C++; tab-width: 4; indent-tabs-mode: t; c-basic-offset: 4 -*- vim:set ts=4 sw=4 sts=4 noet: */
#include <cstdio>
#include <cstdlib>
#include <cstring>

#include "db/driver.h"
#include "sedml.hh"

static void Usage(void)
{
	fprintf(stderr, "usage: flint-sedml DB\n");
}

int main(int argc, char *argv[])
{
	using std::strcmp;

	if (argc != 2) {
		Usage();
		return EXIT_FAILURE;
	}
	if (strcmp("-h", argv[1]) == 0 || strcmp("--help", argv[1]) == 0) {
		Usage();
		return EXIT_SUCCESS;
	}

	db::Driver driver(argv[1]);
	return sedml::Read(driver.db()) ? EXIT_SUCCESS : EXIT_FAILURE;
}
