/* -*- Mode: C++; tab-width: 4; indent-tabs-mode: t; c-basic-offset: 4 -*- vim:set ts=4 sw=4 sts=4 noet: */
#include <cassert>
#include <cstdio>
#include <cstdlib>
#include <cstring>
#include <iostream>

#include "cellml/parser.h"
#include "cellml/translator.h"

using std::cerr;
using std::endl;
using std::strcmp;

namespace {

void Usage()
{
	cerr << "usage: flint-cellml DB IV FUNCTION ODE" << endl;
}

} // namespace

int main(int argc, char *argv[])
{
	static const int kNumOfArgs = 5;

	if (argc == 2) {
		Usage();
		if (strcmp("-h", argv[1]) == 0 || strcmp("--help", argv[1]) == 0) {
			return EXIT_SUCCESS;
		}
		return EXIT_FAILURE;
	}
	if (argc != kNumOfArgs) {
		Usage();
		return EXIT_FAILURE;
	}

	if (!ParseCellml(argv[1]))
		return EXIT_FAILURE;
	if (!TranslateCellml(argv[1], argv[2], argv[3], argv[4]))
		return EXIT_FAILURE;
	return EXIT_SUCCESS;
}
