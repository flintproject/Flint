/* -*- Mode: C++; tab-width: 4; indent-tabs-mode: t; c-basic-offset: 4 -*- vim:set ts=4 sw=4 sts=4 noet: */
#include "method.h"

#include <cassert>
#include <cstdio>
#include <cstdlib>
#include <cstring>
#include <iostream>
#include <iterator>
#include <sstream>
#include <string>
#include <unordered_map>
#include <vector>

#include <boost/uuid/uuid.hpp>
#include <boost/variant/static_visitor.hpp>

#include "db/ast-inserter.h"
#include "db/query.h"
#include "method/equation-grammar.h"
#include "method/equation-lexer.h"
#include "method/helper.h"
#include "method/parser.h"
#include "method/printer.h"

namespace flint {
namespace method {

namespace {

class VariantPrinter : public boost::static_visitor<> {
public:
	explicit VariantPrinter(std::ostream *os)
		: os_(os)
	{}

	void operator()(const Compound &c) const {
		os_->put('(');
		*os_ << c.keyword;
		for (const auto &child : c.children) {
			os_->put(' ');
			boost::apply_visitor(*this, child);
		}
		os_->put(')');
	}

	void operator()(const std::string &s) const {
		if (s[0] != '%') {
			*os_ << s;
			return;
		}
		if (s == "%time") {
			*os_ << "(plus %time @dt)";
			return;
		}
		*os_ << s << "#0";
	}

	void operator()(int i) const {
		*os_ << i;
	}

	void operator()(const flint::lexer::Rational &r) const {
		*os_ << r.lexeme;
	}

	void operator()(const flint::lexer::Real &r) const {
		*os_ << r.lexeme;
	}

private:
	std::ostream *os_;
};

bool IsDifferential(const Expr &e, std::string &id)
{
	if (e.which() != kExprIsCompound)
		return false;
	const auto &c = boost::get<Compound>(e);
	if (c.keyword != "$differential")
		return false;
	if (c.children.size() != 1)
		return false;
	const auto &x = c.children.at(0);
	if (x.which() != kExprIsString)
		return false;
	id = boost::get<std::string>(x);
	return true;
}

bool DecomposeSdeRhsTerm(const Expr &term, std::string &id, Expr &factor)
{
	if (term.which() != kExprIsCompound)
		return false;
	const auto &c = boost::get<Compound>(term);
	if (c.keyword != "times")
		return false;
	if (c.children.size() != 2)
		return false;
	const auto &e0 = c.children.at(0);
	const auto &e1 = c.children.at(1);
	if (IsDifferential(e0, id)) {
		factor = e1;
		return true;
	} else if (IsDifferential(e1, id)) {
		factor = e0;
		return true;
	} else {
		return false;
	}
}

bool IsVoidDrift(Expr &drift)
{
	return drift.which() == kExprIsInteger && boost::get<int>(drift) == 0;
}

/*
 * precondition: drift = 0
 */
bool DecomposeSdeRhs(const Expr &rhs, Expr &drift, std::unordered_map<std::string, Expr> &diffusion)
{
	assert(rhs.which() == kExprIsCompound);
	std::string id;
	if (IsDifferential(rhs, id)) {
		if (id == "%time") {
			if (IsVoidDrift(drift)) {
				drift = 1.0;
				return true;
			} else {
				std::cerr << "more than one drift terms" << std::endl;
				return false;
			}
		} else if (diffusion.emplace(id, 1.0).second) {
			return true;
		} else {
			std::cerr << "more than one diffusion terms are unsupported: " << id << std::endl;
			return false;
		}
	}
	Expr factor;
	if (DecomposeSdeRhsTerm(rhs, id, factor)) {
		if (id == "%time") {
			if (IsVoidDrift(drift)) {
				drift = factor;
				return true;
			} else {
				std::cerr << "more than one drift terms" << std::endl;
				return false;
			}
		} else if (diffusion.emplace(id, factor).second) {
			return true;
		} else {
			std::cerr << "more than one diffusion terms are unsupported: " << id << std::endl;
			return false;
		}
	}
	const auto &c = boost::get<Compound>(rhs);
	if (c.keyword != "plus") {
		std::cerr << "plus expected, but got: " << c.keyword << std::endl;
		return false;
	}
	for (const auto &e : c.children) {
		Expr d = 0;
		if (!DecomposeSdeRhs(e, d, diffusion))
			return false;
		// check if there are more than one drift terms
		if (!IsVoidDrift(d)) {
			if (IsVoidDrift(drift)) {
				drift = d;
			} else {
				std::cerr << "more than one drift terms" << std::endl;
				return false;
			}
		}
	}
	return true;
}

class Inserter : db::AstInserter {
public:
	explicit Inserter(sqlite3 *db)
		: db::AstInserter(db)
	{
	}

	bool PrintAndInsert(const boost::uuids::uuid &uuid,
						const Expr &lhs,
						const Expr &rhs)
	{
		std::ostringstream oss;
		if (lhs.which() == kExprIsString) { // LHS: x = ...
			const std::string &id(boost::get<std::string>(lhs));
			oss << id << "#0";
			std::string name = oss.str();
			oss.str("");
			boost::apply_visitor(VariantPrinter(&oss), rhs);
			std::string math = oss.str();
			return Insert(uuid, name.c_str(), math.c_str());
		}
		std::string id;
		if (IsDifferential(lhs, id)) { // LHS: dx = ...
			Expr drift = 0;
			std::unordered_map<std::string, Expr> diffusion;
			if (!DecomposeSdeRhs(rhs, drift, diffusion))
				return false;
			oss << id << "#0";
			std::string name = oss.str();
			oss.str("");
			for (const auto &d : diffusion) {
				(void)d;
				oss << "(plus ";
			}
			if (IsVoidDrift(drift)) {
				oss << id;
			} else {
				oss << "(plus " << id << " (times ";
				boost::apply_visitor(Printer(&oss), drift);
				oss << " @dt))";
			}
			for (const auto &d : diffusion) {
				oss << " (times ";
				boost::apply_visitor(Printer(&oss), d.second);
				oss << " " << d.first << "#1))";
			}
			std::string math = oss.str();
			return Insert(uuid, name.c_str(), math.c_str());
		}
		// LHS: dx/dt = ...
		assert(lhs.which() == kExprIsCompound);
		const Compound &c(boost::get<Compound>(lhs));
		assert(c.children.size() == 2);
		const Expr &e(c.children.at(1));
		assert(e.which() == kExprIsString);
		id = boost::get<std::string>(e);
		oss << id << "#0";
		std::string name = oss.str();
		oss.str("");
		oss << "(plus " << id << " (times @dt ";
		boost::apply_visitor(Printer(&oss), rhs);
		oss << "))";
		std::string math = oss.str();
		return Insert(uuid, name.c_str(), math.c_str());
	}

	bool PrintAndInsertWiener(const boost::uuids::uuid &uuid,
							  const Expr &e)
	{
		std::ostringstream oss;
		assert(e.which() == kExprIsString);
		const std::string &id(boost::get<std::string>(e));
		oss << id << "#1";
		std::string name = oss.str();
		if (!Insert(uuid, name.c_str(), "($gauss_variate 0 (root @dt))"))
			return false;
		oss.str("");
		oss << id << "#0";
		name = oss.str();
		oss.str("");
		oss << "(plus " << id << " " << id << "#1)";
		std::string math = oss.str();
		return Insert(uuid, name.c_str(), math.c_str());
	}
};

}

bool EulerMaruyama(sqlite3 *db, const char *input, sqlite3 *output)
{
	return Parse<2, EquationLexer, EquationGrammar, Inserter>(db, input, output);
}

}
}
