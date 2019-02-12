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
	VariantPrinter(int k, int n, std::ostream *os)
		: k_(k)
		, n_(n)
		, os_(os)
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
			if (n_ == 1) {
				*os_ << "(plus %time @dt)";
			} else {
				*os_ << "(plus %time (divide @dt "
					 << n_
					 << "))";
			}
			return;
		}
		*os_ << s << '#' << k_;
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
	int k_;
	int n_;
	std::ostream *os_;
};

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
		if (lhs.which() == kExprIsString) {
			const std::string &id(boost::get<std::string>(lhs));
			if (!Even(uuid, id, rhs, 2, 2)) return false;
			if (!Even(uuid, id, rhs, 4, 2)) return false;
			if (!Even(uuid, id, rhs, 6, 1)) return false;
			if (!Even(uuid, id, rhs, 0, 1)) return false;
			return true;
		} else {
			assert(lhs.which() == kExprIsCompound);
			const Compound &c(boost::get<Compound>(lhs));
			assert(c.children.size() == 2);
			const Expr &e(c.children.at(1));
			assert(e.which() == kExprIsString);
			const std::string &id(boost::get<std::string>(e));
			std::string name;
			std::string math;

			// #1: k1 = dt * f(t_n, y_n)
			oss.str("");
			oss << id << "#1";
			name = oss.str();
			oss.str("");
			oss << "(times @dt ";
			boost::apply_visitor(Printer(&oss), rhs);
			oss.put(')');
			math = oss.str();
			if (!Insert(uuid, name.c_str(), math.c_str()))
				return false;

			// #2: y1 = y_n + k1/2
			oss.str("");
			oss << id << "#2";
			name = oss.str();
			oss.str("");
			oss << "(plus " << id << " (divide " << id << "#1 2))";
			math = oss.str();
			if (!Insert(uuid, name.c_str(), math.c_str()))
				return false;

			// #3: k2 = dt * f(t_n + dt/2, y1)
			oss.str("");
			oss << id << "#3";
			name = oss.str();
			oss.str("");
			oss << "(times @dt ";
			boost::apply_visitor(VariantPrinter(2, 2, &oss), rhs);
			oss.put(')');
			math = oss.str();
			if (!Insert(uuid, name.c_str(), math.c_str()))
				return false;

			// #4: y2 = y_n + k2/2
			oss.str("");
			oss << id << "#4";
			name = oss.str();
			oss.str("");
			oss << "(plus " << id << " (divide " << id << "#3 2))";
			math = oss.str();
			if (!Insert(uuid, name.c_str(), math.c_str()))
				return false;

			// #5: k3 = dt * f(t_n + dt/2, y2)
			oss.str("");
			oss << id << "#5";
			name = oss.str();
			oss.str("");
			oss << "(times @dt ";
			boost::apply_visitor(VariantPrinter(4, 2, &oss), rhs);
			oss.put(')');
			math = oss.str();
			if (!Insert(uuid, name.c_str(), math.c_str()))
				return false;

			// #6: y3 = y_n + k3
			oss.str("");
			oss << id << "#6";
			name = oss.str();
			oss.str("");
			oss << "(plus " << id << ' ' << id << "#5)";
			math = oss.str();
			if (!Insert(uuid, name.c_str(), math.c_str()))
				return false;

			// #7: k4 = dt * f(t_n + dt, y3)
			oss.str("");
			oss << id << "#7";
			name = oss.str();
			oss.str("");
			oss << "(times @dt ";
			boost::apply_visitor(VariantPrinter(6, 1, &oss), rhs);
			oss.put(')');
			math = oss.str();
			if (!Insert(uuid, name.c_str(), math.c_str()))
				return false;

			// #0: y_(n+1) = y_n + (k1 + 2*k2 + 2*k3 + k4)/6
			oss.str("");
			oss << id << "#0";
			name = oss.str();
			oss.str("");
			oss << "(plus " << id << " (divide (plus " << id << "#1 (plus (times 2 " << id << "#3) (plus (times 2 " << id << "#5) " << id << "#7))) 6))";
			math = oss.str();
			if (!Insert(uuid, name.c_str(), math.c_str()))
				return false;

			return true;
		}
	}

private:
	bool Even(const boost::uuids::uuid &uuid, const std::string &id, const Expr &rhs,
			  int k, int n) {
		std::ostringstream oss;
		oss << id << '#' << k;
		std::string name = oss.str();
		oss.str("");
		boost::apply_visitor(VariantPrinter(k, n, &oss), rhs);
		std::string math = oss.str();
		return Insert(uuid, name.c_str(), math.c_str());
	}
};

}

bool Rk4(sqlite3 *db, const char *input, sqlite3 *output)
{
	return Parse<8, EquationLexer, EquationGrammar, Inserter>(db, input, output);
}

}
}
