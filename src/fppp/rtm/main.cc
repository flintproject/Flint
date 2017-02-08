/* -*- Mode: C++; tab-width: 4; indent-tabs-mode: t; c-basic-offset: 4 -*- vim:set ts=4 sw=4 sts=4 noet: */
#ifdef HAVE_CONFIG_H
#include "config.h"
#endif

#include <cstdio>
#include <cstdlib>
#include <cstring>
#include <iostream>
#include <memory>

#include "isdf/reader.h"

namespace {

void Usage()
{
	std::cerr << "usage: fppp-rtm FILE" << std::endl;
}

}

int main(int argc, char *argv[])
{
	if (argc < 2) {
		Usage();
		return EXIT_FAILURE;
	}
	return EXIT_SUCCESS;
}
