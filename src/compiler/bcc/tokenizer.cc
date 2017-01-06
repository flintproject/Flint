/* -*- Mode: C++; tab-width: 4; indent-tabs-mode: t; c-basic-offset: 4 -*- vim:set ts=4 sw=4 sts=4 noet: */
#ifdef HAVE_CONFIG_H
#include "config.h"
#endif

#include "compiler/bcc/tokenizer.h"

#include <cassert>
#include <cerrno>
#include <cmath>
#include <cstdio>
#include <cstdlib>
#include <cstring>
#include <iostream>

#include "compiler/bcc/token.h"

namespace flint {
namespace compiler {
namespace bcc {

namespace {
#include "compiler/bcc/function.cc"
#include "compiler/bcc/procedure.cc"
}

class TokenizerImpl {
public:
	explicit TokenizerImpl(const char *input);

	int Read(Token *token);

	std::ostream &Dump(std::ostream &os) const;

private:
	int ReadEol(Token *token);
	int ReadDollar(Token *token);
	int ReadId(char c, Token *token);
	int ReadNumber(Token *token);
	int ReadLabel(Token *token);
	int ReadFunction(Token *token);

	const char *input_;
	const char *point_;
};

TokenizerImpl::TokenizerImpl(const char *input)
	: input_(input)
	, point_(input)
{
	assert(input);
}

namespace {

// Prefer this to std::digit() because of locale-independency
bool IsDigit(char c)
{
	return '0' <= c && c <= '9';
}

bool IsUpper(char c)
{
	return 'A' <= c && c <= 'Z';
}

bool IsLower(char c)
{
	return 'a' <= c && c <= 'z';
}

bool IsAlpha(char c)
{
	return IsUpper(c) || IsLower(c);
}

bool IsAlnum(char c)
{
	return IsDigit(c) || IsUpper(c) || IsLower(c);
}

void ReportUnexpectedCharacter(char c)
{
	std::cerr << "found unexpected character: " << c << std::endl;
}

void ReportTruncatedInput()
{
	std::cerr << "found truncated input" << std::endl;
}

}

std::ostream &TokenizerImpl::Dump(std::ostream &os) const
{
	return os << point_;
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
int TokenizerImpl::Read(Token *token)
{
	char c = *point_;
	switch (c) {
	case '\0':
		return 0;
	case '\t':
		ReportUnexpectedCharacter(c);
		return -1;
	case '\n':
	case '\r':
		return ReadEol(token);
	case ' ':
		token->type = Token::Type::kSpace;
		token->lexeme = point_++;
		token->size = 1;
		return 1;
	case '!':
	case '"':
	case '#': // only allowed in id
		ReportUnexpectedCharacter(c);
		return -1;
	case '$':
		return ReadDollar(token);
	case '%': // an initial character for id
		return ReadId(c, token);
	case '&':
	case '\'':
		ReportUnexpectedCharacter(c);
		return -1;
	case '(':
		token->type = Token::Type::kParenOpen;
		token->lexeme = point_++;
		token->size = 1;
		return 1;
	case ')':
		token->type = Token::Type::kParenClose;
		token->lexeme = point_++;
		token->size = 1;
		return 1;
	case '*':
		ReportUnexpectedCharacter(c);
		return -1;
	case '+': // an initial character for number
		return ReadNumber(token);
	case ',':
		ReportUnexpectedCharacter(c);
		return -1;
	case '-': // an initial character for number
	case '.': // an initial character for number
		return ReadNumber(token);
	case '/':
		ReportUnexpectedCharacter(c);
		return -1;
	case '0': case '1': case '2': case '3': case '4':
	case '5': case '6': case '7': case '8': case '9':
		return ReadNumber(token);
	case ':':
		token->type = Token::Type::kColon;
		token->lexeme = point_++;
		token->size = 1;
		return 1;
	case ';':
	case '<':
		ReportUnexpectedCharacter(c);
		return -1;
	case '=':
		token->type = Token::Type::kEqualSign;
		token->lexeme = point_++;
		token->size = 1;
		return 1;
	case '>':
	case '?':
		ReportUnexpectedCharacter(c);
		return -1;
	case '@': // an initial character for id
		return ReadId(c, token);
	case 'A': case 'B': case 'C': case 'D': case 'E': case 'F': case 'G':
	case 'H': case 'I': case 'J': case 'K':
		ReportUnexpectedCharacter(c);
		return -1;
	case 'L':
		return ReadLabel(token);
	case 'M': case 'N':
	case 'O': case 'P': case 'Q': case 'R': case 'S': case 'T': case 'U':
	case 'V': case 'W': case 'X': case 'Y': case 'Z':
	case '[':
	case '\\':
	case ']':
	case '^':
	case '_': // only allowed in id
	case '`':
		break;
	case 'a': case 'b': case 'c': case 'd': case 'e': case 'f': case 'g':
	case 'h': case 'i': case 'j': case 'k': case 'l': case 'm': case 'n':
	case 'o': case 'p': case 'q': case 'r': case 's': case 't': case 'u':
	case 'v': case 'w': case 'x': case 'y': case 'z':
		return ReadFunction(token);
	case '{':
	case '|':
	case '}':
	case '~':
	default:
		break;
	}
	ReportUnexpectedCharacter(c);
	return -1;
}

int TokenizerImpl::ReadEol(Token *token)
{
	token->type = Token::Type::kEol;
	token->lexeme = point_++;
	int size = 1;
	while ( char c = *point_ ) {
		if (c != '\n' || c != '\r')
			break;
		++point_, ++size;
	}
	token->size = size;
	return 1;
}

int TokenizerImpl::ReadDollar(Token *token)
{
	token->lexeme = point_++;
	char c = *point_;
	if (c == '\0') {
		ReportTruncatedInput();
		return -1;
	}
	if (IsDigit(c)) {
		++point_;
		int size = 2;
		while ( char c = *point_ ) {
			if (!IsDigit(c))
				break;
			++point_, ++size;
		}
		token->type = Token::Type::kAddress;
		token->size = size;
		return 1;
	}
	if (c == 'i') {
		c = *(++point_);
		if (c == '\0') {
			ReportTruncatedInput();
			return -1;
		}
		if (IsDigit(c)) {
			++point_;
			int size = 3;
			while ( char c = *point_ ) {
				if (!IsDigit(c))
					break;
				++point_, ++size;
			}
			token->type = Token::Type::kIntReg;
			token->size = size;
			return 1;
		}
		ReportUnexpectedCharacter(c);
		return -1;
	}
	if (IsAlpha(c)) {
		int size = 1;
		while ( (c = *(++point_)) ) {
			if ( IsAlnum(c) ||  c == '_' ) {
				++size;
				continue;
			}
			break;
		}
		auto *p = ProcedureHash::in_word_set(token->lexeme + 1, size);
		if (!p) {
			std::cerr << "found unknown procedure: "
					  << token->lexeme
					  << std::endl;
			return -1;
		}
		token->type = p->type;
		token->size = size + 1;
		return 1;
	}
	std::cerr << "$ followed by unexpected string: "
			  << point_
			  << std::endl;
	return -1;
}

int TokenizerImpl::ReadId(char c, Token *token)
{
	token->lexeme = point_++;
	char c1 = *point_;
	if ( !IsAlnum(c1) && c1 != '_' ) {
		std::cerr << c << " followed by unexpected string: "
				  << point_
				  << std::endl;
		return -1;
	}
	++point_;
	int size = 2;
	while ( (c1 = *point_) ) {
		if ( IsAlnum(c1) || c1 == '_' || c1 == ':' || c1 == '#' ) {
			++point_, ++size;
			continue;
		}
		break;
	}
	token->type = Token::Type::kIdentifier;
	token->size = size;
	return 1;
}

int TokenizerImpl::ReadNumber(Token *token)
{
	char *e;
	errno = 0;
	double d = std::strtod(point_, &e);
	if (d == 0 && e == point_) {
		std::cerr << "no conversion was performed: "
				  << point_
				  << std::endl;
		return -1;
	}
	if (errno == ERANGE) {
		if (d == HUGE_VAL || d == -HUGE_VAL) {
			std::cerr << "found overflow: " << point_ << std::endl;
			return -1;
		}
		if (d == 0) {
			std::cerr << "found underflow: " << point_ << std::endl;
			return -1;
		}
	}
	token->type = Token::Type::kNumber;
	token->lexeme = point_;
	token->size = static_cast<int>(e - point_);
	point_ = e;
	return 1;
}

int TokenizerImpl::ReadLabel(Token *token)
{
	token->lexeme = point_++;
	char c = *point_;
	if (!IsDigit(c)) {
		std::cerr << "found invalid label: "
				  << point_
				  << std::endl;
		return -1;
	}
	++point_;
	int size = 2;
	while (IsDigit(*point_))
		++point_, ++size;
	token->type = Token::Type::kLabel;
	token->size = size;
	return 1;
}

int TokenizerImpl::ReadFunction(Token *token)
{
	token->lexeme = point_++;
	int size = 1;
	while ( char c = *point_ ) {
		if ( IsDigit(c) || IsLower(c) ) {
			++point_, ++size;
			continue;
		}
		break;
	}
	auto *p = FunctionHash::in_word_set(token->lexeme, size);
	if (!p) {
		std::cerr << "found unknown function: "
				  << token->lexeme
				  << std::endl;
		return -1;
	}
	token->type = p->type;
	token->size = size;
	return 1;
}


Tokenizer::Tokenizer(const char *input)
	: impl_(new TokenizerImpl(input))
{
	assert(input);
}

Tokenizer::~Tokenizer() = default;

int Tokenizer::operator()(Token *token)
{
	return impl_->Read(token);
}

std::ostream &Tokenizer::Dump(std::ostream &os) const
{
	return impl_->Dump(os);
}

}
}
}
