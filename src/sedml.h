/* -*- Mode: C++; tab-width: 4; indent-tabs-mode: t; c-basic-offset: 4 -*- vim:set ts=4 sw=4 sts=4 noet: */
#ifndef FLINT_SEDML_H_
#define FLINT_SEDML_H_

#include "sqlite3.h"

namespace flint {
namespace sedml {

/*
 * Extract simulation configuration in SEDML.
 * sedml_file is encoded in UTF-8.
 * Return true in case of success, false otherwise.
 */
bool Read(const char *sedml_file, sqlite3 *db);

}
}

#endif
