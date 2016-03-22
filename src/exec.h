/* -*- Mode: C++; tab-width: 4; indent-tabs-mode: t; c-basic-offset: 4 -*- vim:set ts=4 sw=4 sts=4 noet: */
#ifndef FLINT_EXEC_H_
#define FLINT_EXEC_H_

#include "sqlite3.h"

#include "cli.pb.h"

namespace flint {
namespace exec {

/*
 * Return the number of possible combination of parameter values
 * in case of success, 0 otherwise.
 */
int Enum(sqlite3 *db);

/*
 * Return true in case of success, false otherwise.
 */
bool Exec(const cli::ExecOption &option);

}
}

#endif
