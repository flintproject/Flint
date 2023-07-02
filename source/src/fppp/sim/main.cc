/* -*- Mode: C++; tab-width: 4; indent-tabs-mode: t; c-basic-offset: 4 -*- vim:set ts=4 sw=4 sts=4 noet: */
#include <cstdio>
#include <cstdlib>
#include <cstring>
#include <iostream>

#include "cli.pb.h"

#include "bc/binary.h"
#include "run.h"

using namespace flint;

namespace {

const size_t kInputLength = 8192;

void Usage()
{
	std::cerr << "usage: fppp-sim HOST [OUTPUT ...]" << std::endl;
}

}

int main(int argc, char *argv[])
{
	GOOGLE_PROTOBUF_VERIFY_VERSION;

	if (argc < 2) {
		Usage();
		return EXIT_FAILURE;
	}
	if ( std::strcmp(argv[1], "-h") == 0 ||
		 std::strcmp(argv[1], "--help") == 0 ) {
		Usage();
		return EXIT_SUCCESS;
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
	option.set_fppp_host(argv[1]);
	for (int i=2;i<argc;i++)
		option.add_fppp_output(argv[i]);
	return run::Run(option, boost::filesystem::path(".")) ? EXIT_SUCCESS : EXIT_FAILURE;
}
