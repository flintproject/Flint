/* -*- Mode: C; tab-width: 4; indent-tabs-mode: t; c-basic-offset: 4 -*- vim:set ts=4 sw=4 sts=4 noet: */
#include <assert.h>
#include <stdio.h>
#include <stdlib.h>

#include "database.h"

int main(int argc, char *argv[])
{
	if (argc != 3) return EXIT_FAILURE;
	if (!SaveModelFile(argv[1], argv[2])) return EXIT_FAILURE;
	return EXIT_SUCCESS;
}
