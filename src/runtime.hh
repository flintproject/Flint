/* -*- Mode: C++; tab-width: 4; indent-tabs-mode: t; c-basic-offset: 4 -*- vim:set ts=4 sw=4 sts=4 noet: */
#ifndef FLINT_RUNTIME_HH_
#define FLINT_RUNTIME_HH_

#include "sqlite3.h"

namespace flint {
namespace runtime {

/*
 * Return true in case of success, otherwise false.
 */
bool Eval(sqlite3 *db,
		  int seed,
		  const char *layout_file,
		  const char *bc_file,
		  const char *output_file);

/*
 * Return true in case of success, otherwise false.
 */
bool Init(sqlite3 *db,
		  int seed,
		  const char *layout_file,
		  const char *bc_file,
		  const char *output_file);

}
}

#endif
