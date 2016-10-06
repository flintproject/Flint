/* -*- Mode: C++; tab-width: 4; indent-tabs-mode: t; c-basic-offset: 4 -*- vim:set ts=4 sw=4 sts=4 noet: */
#include <cstdio>
#include <cstdlib>
#include <cstring>
#include <iostream>

#include "cli.pb.h"

#include "bc/binary.h"
#include "exec.h"

using std::strcmp;

using namespace flint;

namespace {

void Usage()
{
	std::cerr << "usage: flint-exec" << std::endl;
}

}

int main(int argc, char *argv[])
{
	const size_t kInputLength = 4096;

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
	char input[kInputLength]; // FIXME
	size_t size = std::fread(input, 1, kInputLength, stdin);
	if (size == 0) {
		std::cerr << "failed to read the input" << std::endl;
		return EXIT_FAILURE;
	}
	cli::ExecOption option;
	if (!option.ParseFromArray(input, size)) {
		std::cerr << "failed to parse the input" << std::endl;
		return EXIT_FAILURE;
	}

	return exec::Exec(option) ? EXIT_SUCCESS : EXIT_FAILURE;
}
