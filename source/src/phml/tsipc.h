/* -*- Mode: C++; tab-width: 4; indent-tabs-mode: t; c-basic-offset: 4 -*- vim:set ts=4 sw=4 sts=4 noet: */
#ifndef FLINT_PHML_TSIPC_H_
#define FLINT_PHML_TSIPC_H_

#include "sqlite3.h"

namespace flint {
namespace phml {

/*
 * Fill both tables: channels and tsipcforms.
 * Return true in case of success, false otherwise.
 */
bool Tsipc(sqlite3 *db);

}
}

#endif
