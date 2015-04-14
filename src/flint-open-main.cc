/* -*- Mode: C++; tab-width: 4; indent-tabs-mode: t; c-basic-offset: 4 -*- vim:set ts=4 sw=4 sts=4 noet: */
#ifdef HAVE_CONFIG_H
#include "config.h"
#endif

#include <cstdio>
#include <cstdlib>
#include <cstring>
#include <iostream>

#define BOOST_FILESYSTEM_NO_DEPRECATED
#include <boost/filesystem.hpp>

#include "bc/binary.h"
#include "database.h"
#include "system.h"

using std::cerr;
using std::endl;
using std::fprintf;
using std::strcmp;

namespace {

void Usage()
{
	cerr << "usage: flint-open" << endl;
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

	char given_file[1024]; // FIXME
	size_t s = fread(given_file, 1, 1023, stdin);
	if (s == 0) {
		cerr << "failed to read model file name" << endl;
		return EXIT_FAILURE;
	}
	given_file[s] = '\0';
	if (!SaveGivenFile("model", given_file)) {
		return EXIT_FAILURE;
	}

	FILE *fp = fopen("open.mk", "w");
	if (!fp) {
		perror(argv[0]);
		return EXIT_FAILURE;
	}
	fprintf(fp, ".PHONY: all\n");
	fprintf(fp, "\n");
	fprintf(fp, "all: load.mk\n");
	fprintf(fp, "\t$(MAKE) -f load.mk load\n");
	fprintf(fp, "\n");
	fprintf(fp, "load.mk: file.txt model\n");
	fprintf(fp, "\tflint-loadconfig run < $< > $@\n");
	fprintf(fp, "\n");
	fprintf(fp, "file.txt: model\n");
	fprintf(fp, "\tflint-file $< > $@\n");
	fprintf(fp, "\n");
	fclose(fp);

	return RunSystem("flint-make -j -rs -f open.mk");
}
