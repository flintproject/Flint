/* -*- Mode: C++; tab-width: 4; indent-tabs-mode: t; c-basic-offset: 4 -*- vim:set ts=4 sw=4 sts=4 noet: */
#include <cstdlib>

#include "bc/binary.h"
#include "compiler/bcc.h"
#include "compiler/sort.h"
#include "compiler/tac.h"
#include "db/driver.h"

int main(int argc, char *argv[])
{
	RequestBinaryStdio();

	if (argc < 2)
		return EXIT_FAILURE;

	db::Driver driver(argv[1]);
	if (!compiler::sort::Sort(std::cin, driver.db()))
		return EXIT_FAILURE;
	if (!compiler::tac::Tac(driver.db()))
		return EXIT_FAILURE;
	if (!compiler::bcc::Bcc(driver.db()))
		return EXIT_FAILURE;
	return EXIT_SUCCESS;
}
