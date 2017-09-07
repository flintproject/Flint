/* -*- Mode: C++; tab-width: 4; indent-tabs-mode: t; c-basic-offset: 4 -*- vim:set ts=4 sw=4 sts=4 noet: */
#ifdef HAVE_CONFIG_H
#include "config.h"
#endif

#include <cstdio>
#include <cstdlib>
#include <cstring>
#include <iostream>

#include "csv/export.h"

using namespace flint;

namespace {

void usage()
{
	std::cerr << "usage: csv2isd INPUT OUTPUT" << std::endl;
}

} // namespace

int main(int argc, char *argv[])
{
	if (argc == 2) {
		usage();
		if ( std::strcmp(argv[1], "-h") == 0 || std::strcmp(argv[1], "--help") == 0 ) {
			return EXIT_SUCCESS;
		} else {
			return EXIT_FAILURE;
		}
	}
	if (argc != 3) {
		usage();
		return EXIT_FAILURE;
	}
	return (ExportIsdFromCsv(argv[1], argv[2])) ? EXIT_SUCCESS : EXIT_FAILURE;
}
