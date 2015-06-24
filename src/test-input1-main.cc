/* -*- Mode: C++; tab-width: 4; indent-tabs-mode: t; c-basic-offset: 4 -*- vim:set ts=4 sw=4 sts=4 noet: */
#include <cassert>
#include <cstdio>
#include <cstdlib>

#include "database.h"
#include "db/driver.hh"

int main(int argc, char *argv[])
{
	if (argc != 3) return EXIT_FAILURE;
	db::Driver driver(argv[1]);
	if (!SaveGivenFile(driver.db(), argv[2])) return EXIT_FAILURE;
	return EXIT_SUCCESS;
}
