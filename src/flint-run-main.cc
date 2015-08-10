/* -*- Mode: C++; tab-width: 4; indent-tabs-mode: t; c-basic-offset: 4 -*- vim:set ts=4 sw=4 sts=4 noet: */
#include <cstdio>
#include <cstdlib>
#include <cstring>
#include <iostream>

#include "bc/binary.h"
#include "run.hh"

using std::cerr;
using std::endl;
using std::strcmp;

using namespace flint;

namespace {

const size_t kInputLength = 8192;

void Usage()
{
	cerr << "usage: flint-run" << endl;
}

} // namespace

int main(int argc, char *argv[])
{
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
	char buffer[kInputLength];
	size_t s = std::fread(buffer, 1, kInputLength, stdin);
	if (s == 0) {
		cerr << "failed to read the input" << endl;
		return EXIT_FAILURE;
	}
	return run::Run(buffer, (int)s) ? EXIT_SUCCESS : EXIT_FAILURE;
}
