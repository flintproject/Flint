/* -*- Mode: C++; tab-width: 4; indent-tabs-mode: t; c-basic-offset: 4 -*- vim:set ts=4 sw=4 sts=4 noet: */
#include <cstdio>
#include <cstdlib>
#include <cstring>

#include "db/driver.h"
#include "job.hh"

using std::cerr;
using std::endl;
using std::strcmp;

namespace {

void Usage()
{
	cerr << "flint-generate INPUT OUTPUT" << endl;
}

}

int main(int argc, char *argv[])
{
	if (argc != 3) {
		Usage();
		if ( argc == 2 &&
			 ( strcmp(argv[1], "--help") == 0 || strcmp(argv[1], "-h") == 0 ) )
			return EXIT_SUCCESS;
		return EXIT_FAILURE;
	}

	db::Driver input(argv[1]);
	db::Driver output(argv[2]);
	return (job::Generate(input.db(), output.db())) ? EXIT_SUCCESS : EXIT_FAILURE;
}
