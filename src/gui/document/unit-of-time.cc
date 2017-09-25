/* -*- Mode: C++; tab-width: 4; indent-tabs-mode: t; c-basic-offset: 4 -*- vim:set ts=4 sw=4 sts=4 noet: */
#ifdef HAVE_CONFIG_H
#include "config.h"
#endif

#include "gui/document.h"

#include <cassert>
#include <cstdio>
#include <cstdlib>
#include <cstring>
#include <iostream>
#include <map>
#include <memory>
#include <string>

#include "unit.pb.h"
#include "db/statement-driver.h"

namespace flint {
namespace gui {

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
			wxLogError("failed to bind unit_rowid: %d",  e);
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
			wxLogError("failed to step statement: %d", e);
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
			unit->set_name(std::string(reinterpret_cast<const char *>(name)));
			if (!loader_.Load(rowid, unit.get()))
				return false;
			units->emplace(unit_id, std::move(unit));
		}
		if (e != SQLITE_DONE) {
			wxLogError("failed to step statement: %d", e);
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
		wxLogError("missing unit with unit-id %d", id);
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
					wxLogError("non-positional multiplier: %g", multiplier);
					return false;
				}
				long m = static_cast<long>(multiplier);
				if (static_cast<double>(m) != multiplier) {
					wxLogError("multiplier was truncated: %g", multiplier);
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

bool Document::LoadUnitOfTime(sqlite3 *db)
{
	if (format_ == file::kPhml) {
		UnitMap units;
		{
			UnitMapLoader loader(db);
			if (!loader.Load(&units))
				return false;
		}

		int i = 0;
		long d, n;
		for (UnitMap::const_iterator it=units.begin();it!=units.end();++it) {
			if (IsOfTime(units, it->first, &d, &n)) {
				choices_time_.push_back(it->second->name());
				denominators_time_.push_back(d);
				numerators_time_.push_back(n);
				auto p = ids_time_.emplace(it->first, i++);
				assert(p.second);
			}
		}
	}
	if (choices_time_.empty()) {
		choices_time_.push_back("unit time");
		denominators_time_.push_back(1);
		numerators_time_.push_back(1);
	}
	return true;
}

}
}
