/* -*- Mode: C++; tab-width: 4; indent-tabs-mode: t; c-basic-offset: 4 -*- vim:set ts=4 sw=4 sts=4 noet: */
#ifndef FLINT_METHOD_HELPER_HH_
#define FLINT_METHOD_HELPER_HH_

#include <string>

#include "method.hh"

namespace flint {
namespace method {

void RewriteDelayParam(Compound &x, const Expr &expr);

void RewriteDeltaTime(Compound &x, const std::string &id);

}
}

#endif
