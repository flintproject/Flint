/* -*- Mode: C++; tab-width: 4; indent-tabs-mode: t; c-basic-offset: 4 -*- vim:set ts=4 sw=4 sts=4 noet: */
#ifdef HAVE_CONFIG_H
#include "config.h"
#endif

#include <cstdio>
#include <cstdlib>
#include <cstring>
#include <iostream>
#include <memory>

#include "bc/binary.h"

#include "cli.pb.h"

namespace {

void Usage()
{
	std::cerr << "usage: test-run MODEL OUTPUT [G [S]]" << std::endl;
}

} // namespace

int main(int argc, char *argv[])
{
	GOOGLE_PROTOBUF_VERIFY_VERSION;

	if (argc < 3) {
		Usage();
		return EXIT_FAILURE;
	}

	RequestBinaryStdio();

	cli::RunOption option;
	option.set_model_filename(argv[1]);
	option.set_output_filename(argv[2]);
	if (argc > 3) {
		int n = atoi(argv[3]);
		if (n <= 0) {
			std::cerr << "granularity expects a positive integer, but "
				 << argv[3]
				 << std::endl;
			return EXIT_FAILURE;
		}
		option.set_granularity(n);
	}
	if (argc > 4) {
		option.set_spec_filename(argv[4]);
	}
	int s = option.ByteSize();
	std::unique_ptr<char[]> buffer(new char[s]);
	if (!option.SerializeToArray(buffer.get(), s)) {
		std::cerr << "failed to serialize to array" << std::endl;
		return EXIT_FAILURE;
	}
	if (std::fwrite(buffer.get(), s, 1, stdout) != 1) {
		std::cerr << "failed to write option" << std::endl;
		return EXIT_FAILURE;
	}

	google::protobuf::ShutdownProtobufLibrary();

	return EXIT_SUCCESS;
}
