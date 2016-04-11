/* -*- Mode: C++; tab-width: 4; indent-tabs-mode: t; c-basic-offset: 4 -*- vim:set ts=4 sw=4 sts=4 noet: */
#include "flint/tokenizer.h"

#include <cctype>
#include <cstdio>
#include <cstdlib>
#include <cstring>
#include <iostream>

#include "flint/token.h"

namespace flint {
namespace tokenizer {

class Impl {
public:
	Impl(const char *input);

	int Read(Token *token);

private:
	int ReadIdentifier(const char *p, Token *token);
	int ReadRestOfIdentifier(const char *p, Token *token);
	int ReadKeyword(const char *p, Token *token);
	int ReadRestOfKeyword(const char *p, Token *token);
	int ReadNumber(const char *p, Token *token);
	int ReadNumberWithLeadingZero(const char *p, Token *token);
	int ReadRestOfNumber(const char *p, Token *token);
	int ReadRestOfReal(const char *p, Token *token);
	int ReadExponent(const char *p, Token *token);

	const char *input_;
	const char *point_;
};

Impl::Impl(const char *input)
	: input_(input)
	, point_(input)
{
}

/*
 * Read a token.
 * point_ will be at immediately after the end of its lexeme
 * when this returns a success.
 *
 * Returns 1 in case of success,
 * 0 if end of string is found,
 * -1 otherwise, e.g., an error occurs.
 */
int Impl::Read(Token *token)
{
	while (point_[0] == ' ') ++point_; // skip whitespaces

	char c = point_[0];
	switch (c) {
	case '\0':
		return 0;
	case '(':
	case ')':
		token->type = Token::Type::kParenthesis;
		token->lexeme = point_++;
		token->size = 1;
		return 1;
	case '%':
	case '@':
		return ReadIdentifier(point_++, token);
	case '$':
		return ReadKeyword(point_++, token);
	case '+':
	case '-':
		return ReadNumber(point_++, token);
	case '.':
		return ReadRestOfReal(point_++, token);
	case '0':
		return ReadNumberWithLeadingZero(point_++, token);
	case '1': case '2': case '3': case '4':
	case '5': case '6': case '7': case '8': case '9':
		return ReadRestOfNumber(point_++, token);
	default:
		break;
	}
	if (c == '_' || std::isalpha(c))
		return ReadRestOfKeyword(point_++, token);

	std::cerr << "unexpected character during tokenization: "
			  << c
			  << std::endl;
	return -1;
}

int Impl::ReadIdentifier(const char *p, Token *token)
{
	char c = point_[0];
	if (c == '\0') {
		std::cerr << "unexpected eos in identifier: "
				  << p
				  << std::endl;
		return -1;
	}
	if (c == '_' || std::isalpha(c)) {
		++point_;
		return ReadRestOfIdentifier(p, token);
	}
	std::cerr << "unexpected character in identifier: "
			  << p
			  << std::endl;
	return -1;
}

int Impl::ReadRestOfIdentifier(const char *p, Token *token)
{
	char c = point_[0];
	while ( c == '_' ||
			c == ':' ||
			c == '#' ||
			std::isalnum(c) )
		c = *(++point_);
	token->type = Token::Type::kIdentifier;
	token->lexeme = p;
	token->size = point_ - p;
	return 1;
}

int Impl::ReadKeyword(const char *p, Token *token)
{
	char c = point_[0];
	if (c == '\0') {
		std::cerr << "unexpected eos in keyword: "
				  << p
				  << std::endl;
		return -1;
	}
	if (c == '_' || std::isalpha(c)) {
		++point_;
		return ReadRestOfKeyword(p, token);
	}
	std::cerr << "unexpected character in keyword: "
			  << p
			  << std::endl;
	return -1;
}

int Impl::ReadRestOfKeyword(const char *p, Token *token)
{
	char c = point_[0];
	while ( c == '_' ||
			std::isalnum(c) )
		c = *(++point_);
	token->type = Token::Type::kKeyword;
	token->lexeme = p;
	token->size = point_ - p;
	return 1;
}

int Impl::ReadNumber(const char *p, Token *token)
{
	switch (point_[0]) {
	case '\0':
		std::cerr << "unexpected eos in number: "
				  << p
				  << std::endl;
		return -1;
	case '.':
		++point_;
		return ReadRestOfReal(p, token);
	case '0':
		++point_;
		return ReadNumberWithLeadingZero(p, token);
	case '1': case '2': case '3': case '4':
	case '5': case '6': case '7': case '8': case '9':
		++point_;
		return ReadRestOfNumber(p, token);
	}
	std::cerr << "unexpected character in number: "
			  << p
			  << std::endl;
	return -1;
}

int Impl::ReadRestOfNumber(const char *p, Token *token)
{
 initial:
	switch (point_[0]) {
	case 'e':
	case 'E':
		++point_;
		return ReadExponent(p, token);
	case '.':
		++point_;
		return ReadRestOfReal(p, token);
	case '0': case '1': case '2': case '3': case '4':
	case '5': case '6': case '7': case '8': case '9':
		++point_;
		goto initial;
	default:
		token->type = Token::Type::kInteger;
		token->lexeme = p;
		token->size = point_ - p;
		return 1;
	}
}

int Impl::ReadNumberWithLeadingZero(const char *p, Token *token)
{
	char c = point_[0];
	switch (c) {
	case 'e':
	case 'E':
		++point_;
		return ReadExponent(p, token);
	case '.':
		++point_;
		return ReadRestOfReal(p, token);
	}
	// zero
	token->type = Token::Type::kInteger;
	token->lexeme = p;
	token->size = point_ - p;
	return 1;
}

int Impl::ReadRestOfReal(const char *p, Token *token)
{
	while (std::isdigit(point_[0])) ++point_;

	switch (point_[0]) {
	case 'e':
	case 'E':
		++point_;
		return ReadExponent(p, token);
	}

	token->type = Token::Type::kReal;
	token->lexeme = p;
	token->size = point_ - p;
	return 1;
}

int Impl::ReadExponent(const char *p, Token *token)
{
	bool sign = false;

 initial:
	switch (point_[0]) {
	case '\0':
		std::cerr << "unexpected eos in real: "
				  << p
				  << std::endl;
		return -1;
	case '+': case '-':
		if (sign) {
			std::cerr << "consective sign in real: "
					  << p
					  << std::endl;
			return -1;
		}
		sign = true;
		++point_;
		goto initial;
	case '0': case '1': case '2': case '3': case '4':
	case '5': case '6': case '7': case '8': case '9':
		++point_;
		break;
	default:
		std::cerr << "unexpected character in real: "
				  << p
				  << std::endl;
		return -1;
	}

	while (std::isdigit(point_[0])) ++point_;

	token->type = Token::Type::kReal;
	token->lexeme = p;
	token->size = point_ - p;
	return 1;
}


Tokenizer::Tokenizer(const char *input)
	: impl_(new Impl(input))
{
}

Tokenizer::~Tokenizer() = default;

int Tokenizer::operator()(Token *token)
{
	return impl_->Read(token);
}

}
}
