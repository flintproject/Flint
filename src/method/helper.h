/* -*- Mode: C++; tab-width: 4; indent-tabs-mode: t; c-basic-offset: 4 -*- vim:set ts=4 sw=4 sts=4 noet: */
#ifndef FLINT_METHOD_HELPER_H_
#define FLINT_METHOD_HELPER_H_

#include <string>

#include "method.h"

namespace flint {
namespace method {

void RewriteDelayParam(Compound &x, const Expr &expr);

void RewriteDeltaTime(Compound &x, const std::string &id);

}
}

#endif
