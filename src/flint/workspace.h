/* -*- Mode: C++; tab-width: 4; indent-tabs-mode: t; c-basic-offset: 4 -*- vim:set ts=4 sw=4 sts=4 noet: */
#ifndef FLINT_WORKSPACE_H_
#define FLINT_WORKSPACE_H_

#include <cstddef>

namespace flint {
namespace workspace {

bool CreateSparseFile(const char *filename, size_t size);

bool CreateSparseFileAtomically(const char *filename, size_t size);

}
}

#endif
