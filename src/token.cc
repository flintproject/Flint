/* -*- Mode: C++; tab-width: 4; indent-tabs-mode: t; c-basic-offset: 4 -*- vim:set ts=4 sw=4 sts=4 noet: */
#include "flint/token.h"

namespace flint {

bool Token::Equals(const std::string &s) const
{
	return std::string(lexeme, size) == s;
}

std::ostream &Token::Write(std::ostream *os) const
{
	return os->write(lexeme, size);
}

}
