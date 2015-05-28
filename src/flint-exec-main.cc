/* -*- Mode: C++; tab-width: 4; indent-tabs-mode: t; c-basic-offset: 4 -*- vim:set ts=4 sw=4 sts=4 noet: */
#ifdef HAVE_CONFIG_H
#include "config.h"
#endif

#include <cstdio>
#include <cstdlib>
#include <cstring>
#include <iostream>
#include <string>

#define BOOST_FILESYSTEM_NO_DEPRECATED
#include <boost/filesystem.hpp>
#include <boost/scoped_ptr.hpp>

#include "bc/binary.h"
#include "database.h"
#include "db/driver.h"
#include "system.h"

using std::cerr;
using std::endl;
using std::printf;
using std::strcmp;

namespace {

const size_t kInputLength = 2048;

void Usage()
{
	cerr << "usage: flint-exec" << endl;
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

	{
		db::Driver driver("x.db");
		if (!SaveExec(driver.db(), sedml_filename, phsp_filename))
			return EXIT_FAILURE;
	}

	FILE *fp = fopen("exec.mk", "w");
	if (!fp) {
		perror(argv[0]);
		return EXIT_FAILURE;
	}
	fprintf(fp, ".PHONY: all\n");
	fprintf(fp, "\n");
	fprintf(fp, "all: x.mk\n");
	fprintf(fp, "\t$(MAKE) -f $<\n");
	fprintf(fp, "\n");
	fprintf(fp, "x.mk: x.db\n");
	fprintf(fp, "\tflint-sedml $<\n");
	fprintf(fp, "\tflint-phsp $< > $@\n");
	fclose(fp);

	int r = RunSystem("flint-make -rs -f exec.mk");
	if (r != EXIT_SUCCESS) return r;

	boost::system::error_code ec;
	bool b;
	char buf[128]; // FIXME
	for (int i=1;;i++) {
		sprintf(buf, "%d", i);
		boost::filesystem::path dir_path(buf);
		b = boost::filesystem::exists(dir_path, ec);
		if (ec || !b) break;

		sprintf(buf, "%d/canceled", i);
		boost::filesystem::path canceled_path(buf);
		b = boost::filesystem::exists(canceled_path, ec);
		if (!ec && b) continue;

		sprintf(buf, "flint-make -rs -C %d", i);
		r = RunSystem(buf);
		if (r != EXIT_SUCCESS) return r;
	}
	return EXIT_SUCCESS;
}
