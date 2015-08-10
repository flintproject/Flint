/* -*- Mode: C++; tab-width: 4; indent-tabs-mode: t; c-basic-offset: 4 -*- vim:set ts=4 sw=4 sts=4 noet: */
#include <cstdio>
#include <cstdlib>
#include <cstring>
#include <iostream>

#include "bc/binary.h"
#include "exec.hh"

using std::cerr;
using std::endl;
using std::strcmp;

using namespace flint;

namespace {

void Usage()
{
	cerr << "usage: flint-exec" << endl;
}

}

int main(int argc, char *argv[])
{
	const size_t kInputLength = 2048;

	if (argc == 2) {
		Usage();
		if ( strcmp(argv[1], "-h") == 0 ||
			 strcmp(argv[1], "--help") == 0 ) {
			return EXIT_SUCCESS;
		}
		return EXIT_FAILURE;
	}

	RequestBinaryStdio();
	// read input parameters from stdin
	char filenames[kInputLength]; // FIXME
	size_t s = std::fread(filenames, 1, kInputLength, stdin);
	if (s == 0) {
		cerr << "failed to read SEDML/PHSP file names" << endl;
		return EXIT_FAILURE;
	}
	int num_nulls = 0;
	for (size_t i=0;i<s;i++) {
		if (!filenames[i]) {
			if (++num_nulls == 2) {
				break;
			}
		}
	}
	if (num_nulls < 2) {
		cerr << "failed to read SEDML/PHSP file names: " << num_nulls << endl;
		return EXIT_FAILURE;
	}
	const char *sedml_filename = filenames;
	const char *phsp_filename = filenames;
	while (*phsp_filename++) {}

	return exec::Exec(sedml_filename, phsp_filename) ? EXIT_SUCCESS : EXIT_FAILURE;
}
