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
#include "filter/writer.h"
#include "flint/bc.h"
#include "flint/ls.h"
#include "lo/layout.h"
#include "load.h"
#include "task.h"

using namespace flint;

namespace {

void Usage()
{
	std::cerr << "usage: flint-open" << std::endl;
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

	char given_file[1024]; // FIXME
	size_t s = fread(given_file, 1, 1023, stdin);
	if (s == 0) {
		std::cerr << "failed to read model file name" << std::endl;
		return EXIT_FAILURE;
	}
	given_file[s] = '\0';
	std::vector<double> data;
	{
		std::unique_ptr<task::Task> task(load::Load(given_file, load::kOpen, 0, &data));
		if (!task)
			return EXIT_FAILURE;
	}
	// save initial values as init
	FILE *fp = std::fopen("init", "wb");
	if (!fp) {
		std::perror("init");
		return EXIT_FAILURE;
	}
	if (std::fwrite(data.data(), sizeof(double), data.size(), fp) != data.size()) {
		std::cerr << "failed to write init" << std::endl;
		std::fclose(fp);
		return EXIT_FAILURE;
	}
	std::fclose(fp);
	return EXIT_SUCCESS;
}
