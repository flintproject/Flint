/* -*- Mode: C++; tab-width: 4; indent-tabs-mode: t; c-basic-offset: 4 -*- vim:set ts=4 sw=4 sts=4 noet: */
#ifdef HAVE_CONFIG_H
#include "config.h"
#endif

#include <cstdio>
#include <cstdlib>
#include <cstring>
#include <iostream>

#include "bc/binary.h"
#include "load.hh"
#include "workspace/task.h"

using std::cerr;
using std::endl;
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

	workspace::Task task(given_file);
	file::Format format;
	if (!task.Setup(&format))
		return EXIT_FAILURE;
	return load::Load(format, load::kRun) ? EXIT_SUCCESS : EXIT_FAILURE;
}
