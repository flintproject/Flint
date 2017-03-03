/* -*- Mode: C++; tab-width: 4; indent-tabs-mode: t; c-basic-offset: 4 -*- vim:set ts=4 sw=4 sts=4 noet: */
#ifdef HAVE_CONFIG_H
#include "config.h"
#endif

#include "gui/formula/parser.h"

#include <cassert>
#include <cstring>
#include <deque>
#include <unordered_set>
#include <vector>

#include "gui/formula/lexer.h"
#include "gui/formula/token.h"
#include "gui/formula/tree.h"

namespace flint {
namespace gui {
namespace formula {

class ParserImpl {
public:
	ParserImpl(const char *input, std::ostream &es);

	Tree *Parse();

private:
	typedef bool (*TerminatorPredicate)(Token::Type);

	Tree *ParseFormula(TerminatorPredicate tpred,
					   Token::Type *term = nullptr);
	Tree *ParseRestOfFormula(TerminatorPredicate tpred,
							 Token::Type *term,
							 Tree *tree);
	Tree *ParseProduct(TerminatorPredicate tpred,
					   Token::Type *term = nullptr);
	Tree *ParseRestOfProduct(TerminatorPredicate tpred,
							 Token::Type *term,
							 Tree *tree);
	Tree *ParseFactor();
	Tree *ParseGroup();
	bool ParseArguments(std::vector<std::unique_ptr<Tree> > *v);

	Lexer lexer_;
	std::ostream &es_;
	std::deque<Token> tokens_;
};

ParserImpl::ParserImpl(const char *input, std::ostream &es)
	: lexer_(input, es)
	, es_(es)
{}

Tree *ParserImpl::Parse()
{
	for (;;) {
		Token token;
		int n = lexer_(&token);
		if (n == 0)
			break;
		if (n == -1)
			return nullptr;
		tokens_.push_back(token);
	}
	if (tokens_.empty()) {
		es_ << "empty input" << std::endl;
		return nullptr;
	}
	// append an artificial token as terminator
	tokens_.emplace_back();
	tokens_.back().type = Token::Type::kUnspecified;
	return ParseFormula([](Token::Type tt){return tt == Token::Type::kUnspecified;});
}

Tree *ParserImpl::ParseFormula(TerminatorPredicate tpred,
							   Token::Type *term)
{
	assert(!tokens_.empty());
	auto tt = tokens_.front().type;
	if (tt == Token::Type::kUnspecified) {
		es_ << "unexpected eos" << std::endl;
		return nullptr;
	}
	if (tpred(tt)) {
		es_ << "truncated formula: " << tokens_.front().ToString() << std::endl;
		return nullptr;
	}
	if (tt == Token::Type::kPlus) {
		tokens_.pop_front();
		// ignore the leading '+'
		return ParseProduct(tpred, term);
	} else if (tt == Token::Type::kMinus) {
		auto token = tokens_.front();
		tokens_.pop_front();
		std::unique_ptr<Tree> product(ParseProduct(tpred, term));
		if (!product)
			return nullptr;
		std::unique_ptr<Tree> tree(new Tree);
		tree->op = Tree::Op::kMinus;
		tree->token = token;
		tree->children.emplace_back(product.release());
		return tree.release();
	}
	std::unique_ptr<Tree> product(ParseProduct(tpred));
	if (!product)
		return nullptr;
	return ParseRestOfFormula(tpred, term, product.release());
}

Tree *ParserImpl::ParseRestOfFormula(TerminatorPredicate tpred,
									 Token::Type *term,
									 Tree *tree)
{
	assert(!tokens_.empty());
	std::unique_ptr<Tree> left(tree);
	auto tt = tokens_.front().type;
	if (tpred(tt)) {
		tokens_.pop_front();
		if (term)
			*term = tt;
		return left.release();
	}
	if (tt == Token::Type::kUnspecified) {
		es_ << "unexpected eos" << std::endl;
		return nullptr;
	}
	if ( tt == Token::Type::kPlus ||
		 tt == Token::Type::kMinus ) {
		auto token = tokens_.front();
		tokens_.pop_front();
		std::unique_ptr<Tree> right(ParseProduct(tpred));
		if (!right)
			return nullptr;
		std::unique_ptr<Tree> additive(new Tree);
		additive->op = (tt == Token::Type::kPlus) ? Tree::Op::kPlus : Tree::Op::kMinus;
		additive->token = token;
		additive->children.emplace_back(left.release());
		additive->children.emplace_back(right.release());
		return ParseRestOfFormula(tpred, term, additive.release());
	}
	es_ << "failed to parse formula: "
		<< tokens_.front().ToString()
		<< std::endl;
	return nullptr;
}

Tree *ParserImpl::ParseProduct(TerminatorPredicate tpred,
							   Token::Type *term)
{
	std::unique_ptr<Tree> factor(ParseFactor());
	if (!factor)
		return nullptr;
	return ParseRestOfProduct(tpred, term, factor.release());
}

Tree *ParserImpl::ParseRestOfProduct(TerminatorPredicate tpred,
									 Token::Type *term,
									 Tree *tree)
{
	assert(!tokens_.empty());
	std::unique_ptr<Tree> left(tree);
	auto tt = tokens_.front().type;
	if (tpred(tt)) {
		if (term)
			*term = tt;
		return left.release();
	}
	if (tt == Token::Type::kUnspecified) {
		es_ << "unexpected eos" << std::endl;
		return nullptr;
	}
	std::unique_ptr<Tree> product(new Tree);
	switch (tt) {
	case Token::Type::kUnspecified:
		assert(false);
		break;
	case Token::Type::kPercent:
		product->op = Tree::Op::kRem;
		break;
	case Token::Type::kPlus:
	case Token::Type::kMinus:
		return left.release();
	case Token::Type::kStar:
		product->op = Tree::Op::kTimes;
		break;
	case Token::Type::kSlash:
		product->op = Tree::Op::kDivide;
		break;
	default:
		es_ << "failed to parse product: "
			<< tokens_.front().ToString()
			<< std::endl;
		return nullptr;
	}
	tokens_.pop_front();
	std::unique_ptr<Tree> right(ParseFactor());
	if (!right)
		return nullptr;
	product->children.emplace_back(left.release());
	product->children.emplace_back(right.release());
	return ParseRestOfProduct(tpred, term, product.release());
}

namespace {
#include "gui/formula/function.cc"
}

Tree *ParserImpl::ParseFactor()
{
	assert(!tokens_.empty());
	auto tt = tokens_.front().type;
	if (tt == Token::Type::kUnspecified) {
		es_ << "unexpected eos" << std::endl;
		return nullptr;
	}
	std::unique_ptr<Tree> tree(new Tree);
	switch (tt) {
	case Token::Type::kUnspecified:
		assert(false);
		return nullptr;
	case Token::Type::kIdentifier:
		{
			auto token = tokens_.front();
			tokens_.pop_front();
			assert(!tokens_.empty());
			tt = tokens_.front().type;
			if (tt != Token::Type::kParenOpen) {
				tree->op = Tree::Op::kCi;
				tree->token = token;
				return tree.release();
			}
			tokens_.pop_front();
			auto *p = FunctionHash::in_word_set(token.lexeme, token.size);
			if (!p) {
				es_ << "unknown function: "
					<< token.ToString()
					<< std::endl;
				return nullptr;
			}
			tree->op = p->op;
			tree->token = token;
			if (!ParseArguments(&tree->children))
				return nullptr;
			return tree.release();
		}
		break;
	case Token::Type::kNumber:
		tree->op = Tree::Op::kCn;
		tree->token = tokens_.front();
		tokens_.pop_front();
		return tree.release();
	case Token::Type::kParenOpen:
		tokens_.pop_front();
		return ParseGroup();
	case Token::Type::kParenClose:
		es_ << "found right parenthesis without corresponding left one" << std::endl;
		return nullptr;
	case Token::Type::kComma:
		es_ << "found invalid comma" << std::endl;
		return nullptr;
	case Token::Type::kPercent:
		es_ << "found invalid %" << std::endl;
		return nullptr;
	case Token::Type::kStar:
		es_ << "found invalid *" << std::endl;
		return nullptr;
	case Token::Type::kPlus:
		es_ << "found invalid +" << std::endl;
		return nullptr;
	case Token::Type::kMinus:
		es_ << "found invalid -" << std::endl;
		return nullptr;
	case Token::Type::kSlash:
		es_ << "found invalid /" << std::endl;
		return nullptr;
	}
	assert(false);
}

Tree *ParserImpl::ParseGroup()
{
	return ParseFormula([](Token::Type tt){return tt == Token::Type::kParenClose;});
}

namespace {

bool TerminatesArgument(Token::Type tt)
{
	return tt == Token::Type::kComma || tt == Token::Type::kParenClose;
}

}

bool ParserImpl::ParseArguments(std::vector<std::unique_ptr<Tree> > *v)
{
	assert(!tokens_.empty());
	auto tt = tokens_.front().type;
	if (tt == Token::Type::kParenClose)
		return true;
	std::unique_ptr<Tree> arg(ParseFormula(&TerminatesArgument, &tt));
	if (!arg)
		return false;
	v->emplace_back(arg.release());
	if (tt == Token::Type::kParenClose)
		return true;
	return ParseArguments(v);
}


Parser::Parser(const char *input, std::ostream &es)
	: impl_(new ParserImpl(input, es))
{}

Parser::~Parser() = default;

Tree *Parser::operator()()
{
	return impl_->Parse();
}

}
}
}
