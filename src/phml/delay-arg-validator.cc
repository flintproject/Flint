/* -*- Mode: C++; tab-width: 4; indent-tabs-mode: t; c-basic-offset: 4 -*- vim:set ts=4 sw=4 sts=4 noet: */
#include "delay-arg-validator.h"

#include <cassert>
#include <cstdio>
#include <cstdlib>
#include <cstring>
#include <iostream>
#include <memory>
#include <string>
#include <vector>

#include <boost/uuid/uuid.hpp>
#include <boost/uuid/uuid_io.hpp>

#include "db/statement-driver.h"
#include "flint/sexp.h"
#include "flint/sexp/parser.h"

using std::cerr;
using std::endl;

namespace flint {
namespace phml {

namespace {

class Visitor : public sexp::Visitor<bool> {
public:
	Visitor(sqlite3_int64 rowid, const boost::uuids::uuid &uuid, sqlite3 *db)
		: rowid_(rowid)
		, uuid_(uuid)
		, driver_(db,
				  "SELECT max_delay FROM pqs WHERE module_rowid = ? AND name = ?")
	{
	}

	bool operator()(const sexp::Identifier &/*x*/) {return true;}

	bool operator()(const sexp::Literal &/*a*/) {return true;}

	bool operator()(const sexp::Compound &c) {
		const auto &children = c.children();
		size_t s = children.size();
		assert(s > 0);
		const auto &head = children.at(0);
		if (head->type() == sexp::Expression::Type::kIdentifier) {
			const auto &t = static_cast<const sexp::Identifier *>(head.get())->token();
			if (!t.Equals("$Delay"))
				goto next;
			if (s != 3) {
				std::cerr << "invalid number of arguments for Delay()/DeltaTime(): "
						  << (s-1)
						  << std::endl;
				return false;
			}
			const auto &child1 = children.at(1);
			if (child1->type() != sexp::Expression::Type::kIdentifier) {
				std::cerr << "invalid 1st argument for Delay()/DeltaTime()" << std::endl;
				return false;
			}
			const auto &t1 = static_cast<const sexp::Identifier *>(child1.get())->token();
			if (t1.type != sexp::Token::Type::kIdentifier) {
				std::cerr << "invalid 1st argument for Delay()/DeltaTime()" << std::endl;
				return false;
			}
			if (!ValidateArgument(std::string(t1.lexeme, t1.size)))
				return false;
			return sexp::ApplyVisitor(*this, *children.at(2));
		}
	next:
		for (size_t i=1;i<s;i++)
			if (!sexp::ApplyVisitor(*this, *children.at(i)))
				return false;
		return true;
	}

private:
	bool ValidateArgument(const std::string &id) {
		int e;
		e = sqlite3_bind_int64(driver_.stmt(), 1, rowid_);
		if (e != SQLITE_OK) {
			cerr << "failed to bind module_rowid: " << e << endl;
			return false;
		}
		const char *name = id.c_str()+1; // skip the leading '%'
		e = sqlite3_bind_text(driver_.stmt(), 2, name, -1, SQLITE_STATIC);
		if (e != SQLITE_OK) {
			cerr << "failed to bind name: " << e << endl;
			return false;
		}
		e = sqlite3_step(driver_.stmt());
		switch (e) {
		case SQLITE_ROW:
			{
				const unsigned char *max_delay = sqlite3_column_text(driver_.stmt(), 0);
				if (!max_delay) {
					cerr << name
						 << " is given as 1st argument of Delay()/DeltaTime(), but it lacks <max-delay>"
						 << endl
						 << " in " << uuid_
						 << endl;
					return false;
				}
			}
			sqlite3_reset(driver_.stmt());
			return true;
		case SQLITE_DONE:
			cerr << "Delay()/DeltaTime()'s 1st argument must be a PQ name in the same module, but got: "
				 << name << endl
				 << " in " << uuid_
				 << endl;
			return false;
		default:
			cerr << "failed to step statement: " << e << endl;
			return false;
		}
	}

	sqlite3_int64 rowid_;
	boost::uuids::uuid uuid_;
	db::StatementDriver driver_;
};

class Parser {
public:
	explicit Parser(sqlite3 *db)
		: db_(db)
	{
	}

	bool Parse(sqlite3_int64 module_rowid, const boost::uuids::uuid &u, const char *math) {
		std::unique_ptr<sexp::Expression> expr;
		sexp::parser::Parser parser(math);
		if (parser(&expr) <= 0)
			return false;
		Visitor visitor(module_rowid, u, db_);
		return sexp::ApplyVisitor(visitor, *expr);
	}

private:
	sqlite3 *db_;
};

int Process(void *data, int argc, char **argv, char **names)
{
	(void)names;
	assert(argc == 3);
	assert(argv[0]);
	assert(argv[1]);
	Parser *parser = static_cast<Parser *>(data);
	boost::uuids::uuid u;
	std::memcpy(&u, argv[1], u.size());
	return (parser->Parse(std::atol(argv[0]), u, argv[2])) ? 0 : 1;
}

bool IsValid(const char *query, Parser *parser, sqlite3 *db)
{
	char *em;
	int e = sqlite3_exec(db, query, Process, parser, &em);
	switch (e) {
	case SQLITE_OK:
		return true;
	case SQLITE_ABORT: // the callback returns non-zero
		return false;
	default:
		cerr << "failed to exec: " << e
			 << ": " << em << endl;
		sqlite3_free(em);
		return false;
	}
}

}

DelayArgValidator::DelayArgValidator(sqlite3 *db)
	: db_(db)
{
}

bool DelayArgValidator::Validate()
{
	std::unique_ptr<Parser> parser(new Parser(db_));
	if (!IsValid("SELECT p.module_rowid, m.module_id, i.math FROM impls AS i"
				 " LEFT JOIN pqs AS p ON i.pq_rowid = p.rowid"
				 " LEFT JOIN modules AS m ON p.module_rowid = m.rowid",
				 parser.get(), db_))
		return false;
	if (!IsValid("SELECT p.module_rowid, m.module_id, e.math FROM extras AS e"
				 " LEFT JOIN pqs AS p ON e.pq_rowid = p.rowid"
				 " LEFT JOIN modules AS m ON p.module_rowid = m.rowid",
				 parser.get(), db_))
		return false;
	return true;
}

}
}
