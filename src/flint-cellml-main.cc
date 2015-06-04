/* -*- Mode: C++; tab-width: 4; indent-tabs-mode: t; c-basic-offset: 4 -*- vim:set ts=4 sw=4 sts=4 noet: */
#include <cassert>
#include <cstdio>
#include <cstdlib>
#include <cstring>
#include <iostream>

#include "cellml/parser.h"
#include "cellml/translator.h"
#include "db.hh"
#include "db/driver.h"

using std::cerr;
using std::endl;
using std::strcmp;

namespace {

void Usage()
{
	cerr << "usage: flint-cellml DB" << endl;
}

} // namespace

int main(int argc, char *argv[])
{
	static const int kNumOfArgs = 2;

	if (argc != kNumOfArgs) {
		Usage();
		return EXIT_FAILURE;
	}
	if (strcmp("-h", argv[1]) == 0 || strcmp("--help", argv[1]) == 0) {
		Usage();
		return EXIT_SUCCESS;
	}
	db::Driver driver(argv[1]);
	sqlite3 *db = driver.db();
	if (!ParseCellml(db))
		return EXIT_FAILURE;
	if (!TranslateCellml(db))
		return EXIT_FAILURE;
	if (!db::Flow(db))
		return EXIT_FAILURE;
	return EXIT_SUCCESS;
}
