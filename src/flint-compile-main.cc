/* -*- Mode: C++; tab-width: 4; indent-tabs-mode: t; c-basic-offset: 4 -*- vim:set ts=4 sw=4 sts=4 noet: */
#include <cstdlib>
#include <cstdio>

#include "bc/binary.h"
#include "compiler/bcc.h"
#include "compiler/sort.h"
#include "compiler/tac.h"
#include "db/driver.h"
#include "db/read-only-driver.hh"
#include "method.hh"

using std::cerr;
using std::endl;
using std::strcmp;

namespace {

void Usage()
{
	cerr << "usage: flint-compile INPUT TABLE METHOD [OUTPUT]" << endl;
}

}

int main(int argc, char *argv[])
{
	RequestBinaryStdio();

	if (argc != 4 && argc != 5) {
		Usage();
		if ( argc == 2 &&
			 ( strcmp(argv[1], "-h") == 0 ||
			   strcmp(argv[1], "--h") == 0 ) )
			return EXIT_SUCCESS;
		return EXIT_FAILURE;
	}

	db::ReadOnlyDriver input(argv[1]);
	const char *table = argv[2];
	const char *method = argv[3];

	const char *db_file;
	if (argc == 4) {
		db_file = "";
	} else {
		db_file = argv[4];
	}

	db::Driver output(db_file);
	if (strcmp("assign", method) == 0) {
		if (!method::Assign(input.db(), table, output.db()))
			return EXIT_FAILURE;
	} else if (strcmp("euler", method) == 0) {
		if (!method::Euler(input.db(), table, output.db()))
			return EXIT_FAILURE;
	} else if (strcmp("event", method) == 0) {
		if (!method::Event(input.db(), table, output.db()))
			return EXIT_FAILURE;
	} else if (strcmp("rk4", method) == 0) {
		if (!method::Rk4(input.db(), table, output.db()))
			return EXIT_FAILURE;
	} else {
		cerr << "unknown method: " << method << endl;
		return EXIT_FAILURE;
	}
	if (!compiler::sort::Sort(output.db()))
		return EXIT_FAILURE;
	if (!compiler::tac::Tac(output.db()))
		return EXIT_FAILURE;
	if (!compiler::bcc::Bcc(output.db()))
		return EXIT_FAILURE;
	return EXIT_SUCCESS;
}
