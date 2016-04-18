/* -*- Mode: C++; tab-width: 4; indent-tabs-mode: t; c-basic-offset: 4 -*- vim:set ts=4 sw=4 sts=4 noet: */
#ifndef FLINT_SEXP_H_
#define FLINT_SEXP_H_

#include <memory>
#include <vector>

#include "flint/token.h"

namespace flint {
namespace sexp {

class Expression {
public:
	enum class Type {
		kIdentifier,
		kLiteral,
		kCompound
	};

	explicit Expression(Type type);

	virtual ~Expression();

	Type type() const {return type_;}

protected:
	Type type_;
};

class Identifier : public Expression {
public:
	explicit Identifier(const Token &token);

	virtual ~Identifier();

	const Token &token() const {return token_;}

private:
	Token token_;
};


class Literal : public Expression {
public:
	explicit Literal(const Token &token);

	virtual ~Literal();

	const Token &token() const {return token_;}

private:
	Token token_;
};

class Compound : public Expression {
public:
	explicit Compound(std::vector<std::unique_ptr<Expression> > &&children);

	virtual ~Compound();

	size_t GetSize() const;

private:
	std::vector<std::unique_ptr<Expression> > children_;
};

}
}

#endif
