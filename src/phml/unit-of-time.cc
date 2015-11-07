/* -*- Mode: C++; tab-width: 4; indent-tabs-mode: t; c-basic-offset: 4 -*- vim:set ts=4 sw=4 sts=4 noet: */
#ifdef HAVE_CONFIG_H
#include "config.h"
#endif

#include "phml.hh"

#include <cassert>
#include <cstdio>
#include <cstdlib>
#include <cstring>
#include <fstream>
#include <iostream>
#include <map>
#include <memory>
#include <string>

#include "ipc.pb.h"
#include "unit.pb.h"
#include "bc/pack.h"
#include "db/statement-driver.hh"

using std::cerr;
using std::endl;

namespace flint {
namespace phml {

namespace {

class ElementLoader : db::StatementDriver {
public:
	// Note that db is for read only.
	explicit ElementLoader(sqlite3 *db)
		: db::StatementDriver(db, "SELECT unit_id, exponent, factor, multiplier, offset FROM elements WHERE unit_rowid = ?")
	{
	}

	bool Load(sqlite3_int64 rowid, unit::Unit *unit) {
		int e = sqlite3_bind_int64(stmt(), 1, rowid);
		if (e != SQLITE_OK) {
			cerr << "failed to bind unit_rowid: " << e << endl;
			return false;
		}
		for (e = sqlite3_step(stmt()); e == SQLITE_ROW; e = sqlite3_step(stmt())) {
			int unit_id = sqlite3_column_int(stmt(), 0);
			double exponent = sqlite3_column_double(stmt(), 1);
			int factor = sqlite3_column_int(stmt(), 2);
			double multiplier = sqlite3_column_double(stmt(), 3);
			double offset = sqlite3_column_double(stmt(), 4);

			unit::Element *e = unit->add_element();
			e->set_unit_id(unit_id);
			if (factor) e->set_factor(factor);
			if (exponent) e->set_exponent(exponent);
			if (multiplier) e->set_multiplier(multiplier);
			if (offset) e->set_offset(offset);
		}
		if (e != SQLITE_DONE) {
			cerr << "failed to step statement: " << e << endl;
			return false;
		}
		sqlite3_reset(stmt());
		return true;
	}
};

typedef std::map<int, std::unique_ptr<unit::Unit> > UnitMap;

class UnitMapLoader : db::StatementDriver {
public:
	// Note that db is for read only.
	explicit UnitMapLoader(sqlite3 *db)
		: db::StatementDriver(db, "SELECT rowid, unit_id, name FROM units")
		, loader_(db)
	{
	}

	bool Load(UnitMap *units) {
		int e;
		for (e = sqlite3_step(stmt()); e == SQLITE_ROW; e = sqlite3_step(stmt())) {
			sqlite3_int64 rowid = sqlite3_column_int64(stmt(), 0);
			int unit_id = sqlite3_column_int(stmt(), 1);
			const unsigned char *name = sqlite3_column_text(stmt(), 2);
			std::unique_ptr<unit::Unit> unit(new unit::Unit);
			unit->set_id(unit_id);
			unit->set_name(std::string((const char *)name));
			if (!loader_.Load(rowid, unit.get()))
				return false;
			units->insert(std::make_pair(unit_id, std::move(unit)));
		}
		if (e != SQLITE_DONE) {
			cerr << "failed to step statement: " << e << endl;
			return false;
		}
		return true;
	}

private:
	ElementLoader loader_;
};

bool IsOfTime(const UnitMap &units, int id, long *denominator, long *numerator)
{
	UnitMap::const_iterator it = units.find(id);
	if (it == units.end()) {
		cerr << "missing unit with unit-id " << id << endl;
		return false;
	}
	const std::unique_ptr<unit::Unit> &unit = it->second;
	if (unit->name() == "second") {
		*denominator = 1;
		*numerator = 1;
		return true;
	} else if (unit->element_size() == 1) {
		long d, n;
		const unit::Element &element = unit->element(0);
		if (IsOfTime(units, element.unit_id(), &d, &n)) {
			if (element.has_exponent() && element.exponent() != 1) return false;
			if (element.has_offset() && element.offset() != 0) return false;
			if (element.has_factor()) {
				int factor = element.factor();
				if (factor > 0) {
					for (int i=0;i<factor;i++) {
						n *= 10;
					}
				} else if (factor < 0) {
					for (int i=0;i<-factor;i++) {
						d *= 10;
					}
				}
			}
			if (element.has_multiplier()) {
				double multiplier = element.multiplier();
				if (multiplier < 0) {
					cerr << "non-positional multiplier: " << multiplier << endl;
					return false;
				}
				long m = static_cast<long>(multiplier);
				if (static_cast<double>(m) != multiplier) {
					cerr << "multiplier was truncated: " << multiplier << endl;
				}
				n *= m;
			}

			*denominator = d;
			*numerator = n;
			return true;
		}
	}
	return false;
}

}

bool UnitOfTime(sqlite3 *db, const char *output)
{
	UnitMap units;
	{
		UnitMapLoader loader(db);
		if (!loader.Load(&units))
			return false;
	}

	std::ofstream ofs(output, std::ios::binary);
	if (!ofs) {
		cerr << "failed to open " << output << endl;
		return false;
	}
	ipc::TimeUnit tu;
	long d, n;
	for (UnitMap::const_iterator it=units.begin();it!=units.end();++it) {
		if (IsOfTime(units, it->first, &d, &n)) {
			tu.set_name(it->second->name());
			tu.set_d(d);
			tu.set_n(n);
			tu.set_id(it->first);
			if (!PackToOstream(tu, &ofs)) {
				cerr << "failed to pack TimeUnit" << endl;
				return false;
			}
		}
	}
	ofs.close();
	return true;
}

}
}
