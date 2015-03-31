/* -*- Mode: C++; tab-width: 4; indent-tabs-mode: t; c-basic-offset: 4 -*- vim:set ts=4 sw=4 sts=4 noet: */
#ifndef FLINT_BC_BINARY_H_
#define FLINT_BC_BINARY_H_

#ifdef _WIN32
#include <cstdio>
#include <io.h>
#include <fcntl.h>

inline void RequestBinaryStdio() {
	_setmode(_fileno(stdin), _O_BINARY);
	_setmode(_fileno(stdout), _O_BINARY);
}

#else

inline void RequestBinaryStdio() {}

#endif

#endif
