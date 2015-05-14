/* -*- Mode: C++; tab-width: 4; indent-tabs-mode: t; c-basic-offset: 4 -*- vim:set ts=4 sw=4 sts=4 noet: */
#ifndef FLINT_COMPILER_BCC_H_
#define FLINT_COMPILER_BCC_H_

#include "sqlite3.h"

namespace compiler {
namespace bcc {

bool Bcc(sqlite3 *db);

}
}

#endif
