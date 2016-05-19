/* -*- Mode: C++; tab-width: 4; indent-tabs-mode: t; c-basic-offset: 4 -*- vim:set ts=4 sw=4 sts=4 noet: */
#include <cstdio>
#include <cstdlib>
#include <cstring>
#include <iostream>

#include "cli.pb.h"

#include "bc/binary.h"
#include "flint/tr.h"

using namespace flint;

namespace {

const size_t kInputLength = 8192;

void Usage()
{
	std::cerr << "usage: flint-tr" << std::endl;
}

} // namespace

int main(int argc, char *argv[])
{
	if (argc == 2) {
		Usage();
		if ( std::strcmp(argv[1], "-h") == 0 ||
			 std::strcmp(argv[1], "--help") == 0 ) {
			return EXIT_SUCCESS;
		}
		return EXIT_FAILURE;
	}

	RequestBinaryStdio();
	// read input parameters from stdin
	char buffer[kInputLength];
	size_t s = std::fread(buffer, 1, kInputLength, stdin);
	if (s == 0) {
		std::cerr << "failed to read the input" << std::endl;
		return EXIT_FAILURE;
	}
	cli::RunOption option;
	if (!option.ParseFromArray(buffer, s)) {
		std::cerr << "failed to parse the input" << std::endl;
		return EXIT_FAILURE;
	}
	return tr::Translate(option) ? EXIT_SUCCESS : EXIT_FAILURE;
}
