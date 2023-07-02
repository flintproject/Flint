/* -*- Mode: C++; tab-width: 4; indent-tabs-mode: t; c-basic-offset: 4 -*- vim:set ts=4 sw=4 sts=4 noet: */
#ifndef FLINT_CAS_HELPER_H_
#define FLINT_CAS_HELPER_H_

#include <string>

#include "cas.h"

namespace flint {
namespace cas {

void RewriteDeltaTime(Compound &x, const std::string &id);

}
}

#endif
