/* -*- Mode: C++; tab-width: 4; indent-tabs-mode: t; c-basic-offset: 4 -*- vim:set ts=4 sw=4 sts=4 noet: */
#include "flint/sexp.h"

namespace flint {
namespace sexp {

Expression::Expression(Type type)
	: type_(type)
{
}

Expression::~Expression() = default;

Identifier::Identifier(const Token &token)
	: Expression(Type::kIdentifier)
	, token_(token)
{
}

Identifier::~Identifier() = default;

std::ostream &Identifier::Write(std::ostream *os) const
{
	return token_.Write(os);
}

std::string Identifier::GetString() const
{
	return std::string(token_.lexeme, token_.size);
}

Literal::Literal(const Token &token)
	: Expression(Type::kLiteral)
	, token_(token)
{
}

Literal::~Literal() = default;

std::ostream &Literal::Write(std::ostream *os) const
{
	return token_.Write(os);
}

Compound::Compound(std::vector<std::unique_ptr<Expression> > &&children)
	: Expression(Type::kCompound)
	, children_(std::move(children))
{
}

Compound::~Compound() = default;

std::ostream &Compound::Write(std::ostream *os) const
{
	size_t s = children_.size();
	assert(s > 0);
	os->put('(');
	for (size_t i=0;i<s;i++) {
		if (i > 0)
			os->put(' ');
		children_.at(i)->Write(os);
	}
	return os->put(')');
}

size_t Compound::GetSize() const
{
	return children_.size();
}

}
}
