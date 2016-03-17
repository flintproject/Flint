/* -*- Mode: C++; tab-width: 4; indent-tabs-mode: t; c-basic-offset: 4 -*- vim:set ts=4 sw=4 sts=4 noet: */
#ifndef FLINT_PHML_SPRINKLE_H_
#define FLINT_PHML_SPRINKLE_H_

#include "sqlite3.h"

namespace flint {
namespace phml {

bool Sprinkle(sqlite3 *db);

}
}

#endif
