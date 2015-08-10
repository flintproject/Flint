/* -*- Mode: C++; tab-width: 4; indent-tabs-mode: t; c-basic-offset: 4 -*- vim:set ts=4 sw=4 sts=4 noet: */
#ifndef FLINT_METHOD_HH_
#define FLINT_METHOD_HH_

#include <string>
#include <vector>

#include <boost/fusion/include/adapt_struct.hpp>
#include <boost/variant/recursive_variant.hpp>

#include "lexer.hh"
#include "sqlite3.h"

namespace flint {
namespace method {

struct Compound;

enum {
	kExprIsCompound,
	kExprIsString,
	kExprIsInteger,
	kExprIsReal
};

typedef boost::variant<boost::recursive_wrapper<Compound>,
					   std::string,
					   int,
					   flint::lexer::Real
					   > Expr;

struct Compound {
	std::string keyword;
	std::vector<Expr> children;
};

}
}

BOOST_FUSION_ADAPT_STRUCT(flint::method::Compound,
						  (std::string, keyword)
						  (std::vector<flint::method::Expr>, children))

namespace flint {
namespace method {

/*
 * Generate imperative AST for assignment statements.
 * Note that db is for read only.
 * Return true in case of success, false otherwise.
 */
bool Assign(sqlite3 *db, const char *input, sqlite3 *output);

/*
 * Generate imperative AST for event statements.
 * Note that db is for read only.
 * Return true in case of success, false otherwise.
 */
bool Event(sqlite3 *db, const char *input, sqlite3 *output);

/*
 * Generate imperative AST from equations by Euler method.
 * Note that db is for read only.
 * Return true in case of success, false otherwise.
 */
bool Euler(sqlite3 *db, const char *input, sqlite3 *output);

/*
 * Generate imperative AST from equations by Runge-Kutta 4th method.
 * Note that db is for read only.
 * Return true in case of success, false otherwise.
 */
bool Rk4(sqlite3 *db, const char *input, sqlite3 *output);

}
}

#endif
