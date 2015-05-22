/* -*- Mode: C++; tab-width: 4; indent-tabs-mode: t; c-basic-offset: 4 -*- vim:set ts=4 sw=4 sts=4 noet: */
#ifndef FLINT_METHOD_HH_
#define FLINT_METHOD_HH_

#include <string>
#include <vector>

#include <boost/fusion/include/adapt_struct.hpp>
#include <boost/variant/recursive_variant.hpp>

#include "sqlite3.h"

namespace method {

struct Compound;

enum {
	kExprIsCompound,
	kExprIsString,
	kExprIsInt,
	kExprIsDouble
};

typedef boost::variant<boost::recursive_wrapper<Compound>,
					   std::string,
					   int,
					   double
					   > Expr;

struct Compound {
	std::string keyword;
	std::vector<Expr> children;
};

}

BOOST_FUSION_ADAPT_STRUCT(method::Compound,
						  (std::string, keyword)
						  (std::vector<method::Expr>, children))

namespace method {

/*
 * Generate imperative AST for assignment statements.
 * Return true in case of success, false otherwise.
 */
bool Assign(sqlite3 *db, const char *input, const char *output);

/*
 * Generate imperative AST for event statements.
 * Return true in case of success, false otherwise.
 */
bool Event(sqlite3 *db, const char *input, const char *output);

/*
 * Generate imperative AST from equations by Euler method.
 * Return true in case of success, false otherwise.
 */
bool Euler(sqlite3 *db, const char *input, const char *output);

/*
 * Generate imperative AST from equations by Runge-Kutta 4th method.
 * Return true in case of success, false otherwise.
 */
bool Rk4(sqlite3 *db, const char *input, const char *output);

}

#endif
