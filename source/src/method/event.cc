/* -*- Mode: C++; tab-width: 4; indent-tabs-mode: t; c-basic-offset: 4 -*- vim:set ts=4 sw=4 sts=4 noet: */
#include "method.h"

#include <cassert>
#include <cstdio>
#include <iostream>
#include <sstream>
#include <string>

#include <boost/uuid/uuid.hpp>

#include "db/ast-inserter.h"
#include "method/assignment-grammar.h"
#include "method/assignment-lexer.h"
#include "method/parser.h"
#include "method/printer.h"

namespace flint {
namespace method {

namespace {

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
		if (lhs.which() != kExprIsString) {
			assert(false);
			return false;
		}
		const std::string &id(boost::get<std::string>(lhs));
		std::ostringstream oss;
		oss << id << "#0";
		std::string name = oss.str();
		oss.str("");
		boost::apply_visitor(Printer(&oss), rhs);
		std::string math = oss.str();
		return Insert(uuid, name.c_str(), math.c_str());
	}

	bool PrintAndInsertWiener(const boost::uuids::uuid &,
							  const Expr &)
	{
		assert(false);
		return false;
	}
};

}

bool Event(sqlite3 *db, const char *input, sqlite3 *output)
{
	return Parse<1, AssignmentLexer, AssignmentGrammar, Inserter>(db, input, output);
}

}
}
