/* -*- Mode: C++; tab-width: 4; indent-tabs-mode: t; c-basic-offset: 4 -*- vim:set ts=4 sw=4 sts=4 noet: */
#include "method.hh"

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
#include "method/helper.hh"
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
		std::vector<Expr>::const_iterator bit = c.children.begin();
		std::vector<Expr>::const_iterator eit = c.children.end();
		for (std::vector<Expr>::const_iterator it=bit;it!=eit;++it) {
			os_->put(' ');
			boost::apply_visitor(*this, *it);
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

	void operator()(const flint::lexer::Real &r) const {
		*os_ << r.lexeme;
	}

private:
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
			oss << id << "#0";
			std::string name = oss.str();
			oss.str("");
			boost::apply_visitor(VariantPrinter(&oss), rhs);
			std::string math = oss.str();
			return Insert(uuid, name.c_str(), math.c_str());
		} else {
			assert(lhs.which() == kExprIsCompound);
			const Compound &c(boost::get<Compound>(lhs));
			assert(c.children.size() == 2);
			const Expr &e(c.children.at(1));
			assert(e.which() == kExprIsString);
			const std::string &id(boost::get<std::string>(e));
			oss << id << "#0";
			std::string name = oss.str();
			oss.str("");
			oss << "(plus " << id << " (times @dt ";
			boost::apply_visitor(Printer(&oss), rhs);
			oss << "))";
			std::string math = oss.str();
			return Insert(uuid, name.c_str(), math.c_str());
		}
	}
};

}

bool Euler(sqlite3 *db, const char *input, sqlite3 *output)
{
	return Parse<1, EquationLexer, EquationGrammar, Inserter>(db, input, output);
}

}
}
