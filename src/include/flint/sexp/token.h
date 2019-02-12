/* -*- Mode: C++; tab-width: 4; indent-tabs-mode: t; c-basic-offset: 4 -*- vim:set ts=4 sw=4 sts=4 noet: */
#ifndef FLINT_SEXP_TOKEN_H_
#define FLINT_SEXP_TOKEN_H_

#include <ostream>
#include <string>

namespace flint {
namespace sexp {

struct Token {

	enum class Type {
		kParenthesis,
		kIdentifier,
		kKeyword,
		kInteger,
		kRational,
		kReal
	};

	Type type;
	const char *lexeme;
	int size;

	bool Equals(const std::string &s) const;

	std::ostream &Write(std::ostream *os) const;
};

}
}

#endif
