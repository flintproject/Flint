/* -*- Mode: C++; tab-width: 4; indent-tabs-mode: t; c-basic-offset: 4 -*- vim:set ts=4 sw=4 sts=4 noet: */
#ifdef HAVE_CONFIG_H
#include "config.h"
#endif

#include <cassert>
#include <cstdio>
#include <cstdlib>
#include <cstring>

#include "db/driver.h"
#include "phz.hh"

using std::cerr;
using std::endl;
using std::strcmp;

namespace {

void Usage()
{
	cerr << "flint-phz DB TARGETDIR" << endl;
}

} // namespace

int main(int argc, char *argv[])
{
	if (argc == 2) {
		Usage();
		if (strcmp("-h", argv[1]) == 0 || strcmp("--help", argv[1]) == 0) {
			return EXIT_SUCCESS;
		}
		return EXIT_FAILURE;
	}
	if (argc != 3) {
		Usage();
		return EXIT_FAILURE;
	}

	db::Driver driver(argv[1]);
	return phz::Read(driver.db(), argv[2]) ? EXIT_SUCCESS : EXIT_FAILURE;
}
