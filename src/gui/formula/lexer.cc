/* -*- Mode: C++; tab-width: 4; indent-tabs-mode: t; c-basic-offset: 4 -*- vim:set ts=4 sw=4 sts=4 noet: */
#ifdef HAVE_CONFIG_H
#include "config.h"
#endif

#include "gui/formula/lexer.h"

#include <cassert>
#include <cerrno>
#include <cmath>
#include <cstdlib>

#include "gui/formula/token.h"

namespace flint {
namespace gui {
namespace formula {

class LexerImpl {
public:
	LexerImpl(const char *input, std::ostream &es);

	int Read(Token *token);

private:
	int ReadIdentifier(Token *token);
	int ReadNumber(Token *token);

	void ReportUnexpectedCharacter(char c);

	const char *point_;
	std::ostream &es_;
};

LexerImpl::LexerImpl(const char *input, std::ostream &es)
	: point_(input)
	, es_(es)
{
	assert(input);
}

namespace {

// For locale-dependency
bool IsAlpha(char c)
{
	return ('A' <= c && c <= 'Z') || ('a' <= c && c <= 'z');
}

// For locale-dependency
bool IsDigit(char c)
{
	return '0' <= c && c <= '9';
}

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
int LexerImpl::Read(Token *token)
{
	char c = *point_;
	switch (c) {
	case '\0':
		return 0;
	case ' ':
		// ignore space
		++point_;
		return Read(token);
	case '!': case '"': case '#': case '$':
		ReportUnexpectedCharacter(c);
		return -1;
	case '%':
		token->type = Token::Type::kPercent;
		token->lexeme = point_++;
		token->size = 1;
		return 1;
	case '&': case '\'':
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
		token->type = Token::Type::kStar;
		token->lexeme = point_++;
		token->size = 1;
		return 1;
	case '+':
		token->type = Token::Type::kPlus;
		token->lexeme = point_++;
		token->size = 1;
		return 1;
	case ',':
		token->type = Token::Type::kComma;
		token->lexeme = point_++;
		token->size = 1;
		return 1;
	case '-':
		token->type = Token::Type::kMinus;
		token->lexeme = point_++;
		token->size = 1;
		return 1;
	case '.':
		return ReadNumber(token);
	case '/':
		token->type = Token::Type::kSlash;
		token->lexeme = point_++;
		token->size = 1;
		return 1;
	case '0': case '1': case '2': case '3': case '4':
	case '5': case '6': case '7': case '8': case '9':
		return ReadNumber(token);
	case ':': case ';': case '<': case '=': case '>':
	case '?': case '@':
		ReportUnexpectedCharacter(c);
		return -1;
	case 'A': case 'B': case 'C': case 'D': case 'E': case 'F': case 'G':
	case 'H': case 'I': case 'J': case 'K':	case 'L': case 'M': case 'N':
	case 'O': case 'P': case 'Q': case 'R': case 'S': case 'T': case 'U':
	case 'V': case 'W': case 'X': case 'Y': case 'Z':
		return ReadIdentifier(token);
	case '[': case '\\': case ']': case '^':
		ReportUnexpectedCharacter(c);
		return -1;
	case '_':
		return ReadIdentifier(token);
	case '`':
		ReportUnexpectedCharacter(c);
		return -1;
	case 'a': case 'b': case 'c': case 'd': case 'e': case 'f': case 'g':
	case 'h': case 'i': case 'j': case 'k': case 'l': case 'm': case 'n':
	case 'o': case 'p': case 'q': case 'r': case 's': case 't': case 'u':
	case 'v': case 'w': case 'x': case 'y': case 'z':
		return ReadIdentifier(token);
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

int LexerImpl::ReadIdentifier(Token *token)
{
	int i = 1;
	while ( char c = point_[i] ) {
		if ( IsAlpha(c) || IsDigit(c) || c == '_' ) {
			++i;
			continue;
		}
		break;
	}
	token->type = Token::Type::kIdentifier;
	token->lexeme = point_;
	token->size = i;
	point_ += i;
	return 1;
}

int LexerImpl::ReadNumber(Token *token)
{
	char *e;
	errno = 0;
	double d = std::strtod(point_, &e);
	if (d == 0 && e == point_) {
		es_ << "no conversion was performed: "
			<< point_
			<< std::endl;
		return -1;
	}
	if (errno == ERANGE) {
		if (d == HUGE_VAL || d == -HUGE_VAL) {
			es_ << "found overflow: "
				<< point_
				<< std::endl;
			return -1;
		}
		if (d == 0) {
			es_ << "found underflow: "
				<< point_
				<< std::endl;
			return -1;
		}
	}
	token->type = Token::Type::kNumber;
	token->lexeme = point_;
	token->size = static_cast<int>(e - point_);
	point_ = e;
	return 1;
}

void LexerImpl::ReportUnexpectedCharacter(char c)
{
	es_ << "found unexpected character: " << c << std::endl;
}


Lexer::Lexer(const char *input, std::ostream &es)
	: impl_(new LexerImpl(input, es))
{}

Lexer::~Lexer() = default;

int Lexer::operator()(Token *token)
{
	return impl_->Read(token);
}

}
}
}
