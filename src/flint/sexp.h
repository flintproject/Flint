/* -*- Mode: C++; tab-width: 4; indent-tabs-mode: t; c-basic-offset: 4 -*- vim:set ts=4 sw=4 sts=4 noet: */
#ifndef FLINT_SEXP_H_
#define FLINT_SEXP_H_

#include <cassert>
#include <memory>
#include <ostream>
#include <string>
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

	virtual std::ostream &Write(std::ostream *os) const = 0;

protected:
	Type type_;
};

class Identifier : public Expression {
public:
	explicit Identifier(const Token &token);

	virtual ~Identifier();

	virtual std::ostream &Write(std::ostream *os) const override;

	const Token &token() const {return token_;}

	std::string GetString() const;

private:
	Token token_;
};


class Literal : public Expression {
public:
	explicit Literal(const Token &token);

	virtual ~Literal();

	virtual std::ostream &Write(std::ostream *os) const override;

	const Token &token() const {return token_;}

private:
	Token token_;
};

class Compound : public Expression {
public:
	explicit Compound(std::vector<std::unique_ptr<Expression> > &&children);

	virtual ~Compound();

	virtual std::ostream &Write(std::ostream *os) const override;

	size_t GetSize() const;

	const std::vector<std::unique_ptr<Expression> > &children() const {return children_;}

private:
	std::vector<std::unique_ptr<Expression> > children_;
};

template<typename T>
class Visitor {
public:
	virtual T operator()(const Identifier &) = 0;
	virtual T operator()(const Literal &) = 0;
	virtual T operator()(const Compound &) = 0;
};

template<typename T>
T ApplyVisitor(Visitor<T> &visitor, const Expression &expr)
{
	switch (expr.type()) {
	case Expression::Type::kIdentifier:
		return visitor(static_cast<const Identifier &>(expr));
	case Expression::Type::kLiteral:
		return visitor(static_cast<const Literal &>(expr));
	case Expression::Type::kCompound:
		return visitor(static_cast<const Compound &>(expr));
	default:
		assert(false);
		return T();
	}
}

}
}

#endif
