/* -*- Mode: C++; tab-width: 4; indent-tabs-mode: t; c-basic-offset: 4 -*- vim:set ts=4 sw=4 sts=4 noet: */
#ifdef HAVE_CONFIG_H
#include "config.h"
#endif

#include <cassert>
#include <cstdio>
#include <cstdlib>
#include <iostream>
#include <string>

#include <boost/scoped_ptr.hpp>

#include "unit.pb.h"

#include "db/read-only-driver.hh"
#include "db/statement-driver.h"
#include "bc/binary.h"
#include "bc/pack.h"

using std::cerr;
using std::endl;
using std::strcmp;
using std::string;

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

void Usage()
{
	cerr << "usage: flint-unit DB" << endl;
}

} // namespace

int main(int argc, char *argv[])
{
	GOOGLE_PROTOBUF_VERIFY_VERSION;

	RequestBinaryStdio();

	if (argc != 2) {
		Usage();
		return EXIT_FAILURE;
	}
	if (strcmp("-h", argv[1]) == 0 || strcmp("--help", argv[1]) == 0) {
		Usage();
		return EXIT_SUCCESS;
	}

	db::ReadOnlyDriver driver(argv[1]);
	sqlite3_stmt *stmt;
	int e = sqlite3_prepare_v2(driver.db(),
							   "SELECT rowid, unit_id, name FROM units",
							   -1, &stmt, NULL);
	if (e != SQLITE_OK) {
		cerr << "failed to prepare statement: " << e << endl;
		return EXIT_FAILURE;
	}
	{
		ElementLoader loader(driver.db());
		for (e = sqlite3_step(stmt); e == SQLITE_ROW; e = sqlite3_step(stmt)) {
			sqlite3_int64 rowid = sqlite3_column_int64(stmt, 0);
			int unit_id = sqlite3_column_int(stmt, 1);
			const unsigned char *name = sqlite3_column_text(stmt, 2);
			boost::scoped_ptr<unit::Unit> unit(new unit::Unit);
			unit->set_id(unit_id);
			unit->set_name(string((const char *)name));
			if (!loader.Load(rowid, unit.get())) return EXIT_FAILURE;
			if (!PackToOstream(*unit, &std::cout)) {
				cerr << "failed to pack Unit" << endl;
				return EXIT_FAILURE;
			}
		}
		if (e != SQLITE_DONE) {
			cerr << "failed to step statement: " << e << endl;
			return EXIT_FAILURE;
		}
	}
	sqlite3_finalize(stmt);

	google::protobuf::ShutdownProtobufLibrary();

	return EXIT_SUCCESS;
}
