/* -*- Mode: C++; tab-width: 4; indent-tabs-mode: t; c-basic-offset: 4 -*- vim:set ts=4 sw=4 sts=4 noet: */
#ifndef FLINT_SOLVER_H_
#define FLINT_SOLVER_H_

#include <cstdio>

#include "sqlite3.h"

namespace flint {

namespace job {
struct Option;
}

namespace solver {

enum class Method {
	kArk
};

/*
 * Note that db is for read only.
 * Return true in case of success, false otherwise.
 */
bool Solve(sqlite3 *db, Method method, const job::Option &option);

}
}

#endif
