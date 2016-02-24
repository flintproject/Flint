/* -*- Mode: C++; tab-width: 4; indent-tabs-mode: t; c-basic-offset: 4 -*- vim:set ts=4 sw=4 sts=4 noet: */
#ifndef FLINT_SOLVER_ARK_H_
#define FLINT_SOLVER_ARK_H_

#include "sqlite3.h"

namespace flint {

namespace job {
struct Option;
}

namespace solver {

namespace ark {

bool Solve(sqlite3 *db, const job::Option &option);

}
}
}

#endif
