/* -*- Mode: C++; tab-width: 4; indent-tabs-mode: t; c-basic-offset: 4 -*- vim:set ts=4 sw=4 sts=4 noet: */
#ifndef FLINT_DB_TARGET_LOADER_H_
#define FLINT_DB_TARGET_LOADER_H_

#include <cassert>
#include <cstdio>
#include <cstdlib>
#include <cstring>
#include <iostream>

#include <boost/spirit/include/phoenix.hpp>
#include <boost/spirit/include/qi.hpp>
#include <boost/uuid/uuid.hpp>

#include "statement-driver.h"

namespace flint {
namespace db {

class TargetLoader : StatementDriver {
public:
	// Note that db is for read only.
	explicit TargetLoader(sqlite3 *db)
		: StatementDriver(db, "SELECT instances.module_id, tms.module_id, tpqs.pq_id, tpqs.math FROM tpqs LEFT JOIN tms ON tpqs.tm_rowid = tms.rowid LEFT JOIN instances ON tms.instance_rowid = instances.rowid")
	{
	}

	template<typename THandler>
	bool Load(THandler *handler) {
		int e;
		for (e = sqlite3_step(stmt()); e == SQLITE_ROW; e = sqlite3_step(stmt())) {
			const void *uuid0 = sqlite3_column_blob(stmt(), 0);
			const void *uuid1 = sqlite3_column_blob(stmt(), 1);
			int pq_id = sqlite3_column_int(stmt(), 2);
			const unsigned char *math = sqlite3_column_text(stmt(), 3);
			assert(uuid0);
			assert(uuid1);
			boost::uuids::uuid u0, u1;
			std::memcpy(&u0, uuid0, u0.size());
			std::memcpy(&u1, uuid1, u1.size());
			double val;
			if (!Parse((const char *)math, &val)) return false;
			if (!handler->Handle(u0, u1, pq_id, val))
				return false;
		}
		if (e != SQLITE_DONE) {
			std::cerr << "failed to step statement: " << e << std::endl;
			return false;
		}
		sqlite3_reset(stmt());
		return true;
	}

private:
	bool Parse(const char *math, double *val) {
		using boost::phoenix::ref;
		using boost::spirit::_1;
		using boost::spirit::qi::alnum;
		using boost::spirit::qi::char_;
		using boost::spirit::qi::double_;
		using boost::spirit::qi::parse;

		const char *p = math;
		size_t len = std::strlen(math);
		double v;
		bool r = parse(p, math + len,
					   *char_(' ')
					   >> "(eq %"
					   >> *(alnum | char_(':') | char_('_'))
					   >> ' '
					   >> double_[ref(v) = _1]
					   >> ')');
		if (!r || p != math + len) {
			std::cerr << "invalid math of targets: " << math << std::endl;
			return false;
		}
		*val = v;
		return true;
	}
};

}
}

#endif
