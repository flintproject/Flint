/* -*- Mode: C++; tab-width: 4; indent-tabs-mode: t; c-basic-offset: 4 -*- vim:set ts=4 sw=4 sts=4 noet: */
#ifdef HAVE_CONFIG_H
#include "config.h"
#endif

#include <cstdio>
#include <cstdlib>
#include <cstring>
#include <iostream>

#define BOOST_FILESYSTEM_NO_DEPRECATED
#include <boost/filesystem.hpp>
#include <boost/scoped_array.hpp>
#include <boost/scoped_ptr.hpp>

#include "database.h"
#include "db/driver.h"
#include "system.h"
#include "utf8path.h"

using std::cerr;
using std::endl;
using std::printf;
using std::sprintf;
using std::strcmp;
using std::strlen;

namespace {

bool SaveAndPrint(const char *uuid, const char *xml_file)
{
	boost::scoped_array<char> db_file(new char[64]);
	sprintf(db_file.get(), "%s.db", uuid);
	if (!SaveGivenFile(db_file.get(), xml_file)) return false;
	printf("%s\n", uuid);
	return true;
}

void Usage()
{
	cerr << "usage: flint-importdumpall DB" << endl;
}

} // namespace

int main(int argc, char *argv[])
{
	if (argc != 2) {
		Usage();
		return EXIT_FAILURE;
	}
	if (strcmp("-h", argv[1]) == 0 || strcmp("--help", argv[1]) == 0) {
		Usage();
		return EXIT_SUCCESS;
	}

	boost::scoped_ptr<db::Driver> driver(new db::Driver(argv[1]));

	sqlite3_stmt *stmt;
	int e = sqlite3_prepare_v2(driver->db(),
							   "SELECT m.module_id, i.type, i.ref FROM imports AS i LEFT JOIN modules AS m ON i.module_rowid = m.rowid",
							   -1, &stmt, NULL);
	if (e != SQLITE_OK) {
		cerr << "failed to prepare statement: " << e << endl;
		return EXIT_FAILURE;
	}

	size_t len = strlen(argv[1]);
	for (e = sqlite3_step(stmt); e == SQLITE_ROW; e = sqlite3_step(stmt)) {
		const unsigned char *uuid = sqlite3_column_text(stmt, 0);
		const unsigned char *type = sqlite3_column_text(stmt, 1);
		const unsigned char *ref = sqlite3_column_text(stmt, 2);

		if (strcmp((const char *)type, "external") == 0) {
			if (!SaveAndPrint((const char *)uuid, (const char *)ref)) return EXIT_FAILURE;
		} else {
			boost::scoped_array<char> c(new char[len + 256]); // FIXME
			sprintf(c.get(), "flint-importdump %s %s", (const char *)uuid, argv[1]);
			int r = RunSystem(c.get());
			if (r != 0) return r;
			boost::scoped_array<char> xml_file(new char[64]);
			sprintf(xml_file.get(), "%s.xml", uuid);
			boost::filesystem::path xml_path = boost::filesystem::absolute(xml_file.get());
			boost::scoped_array<char> xml_utf8(GetUtf8FromPath(xml_path));
			if (!SaveAndPrint((const char *)uuid, xml_utf8.get())) return EXIT_FAILURE;
		}
	}
	if (e != SQLITE_DONE) {
		cerr << "failed to step statement: " << e << endl;
		return EXIT_FAILURE;
	}
	sqlite3_finalize(stmt);

	return EXIT_SUCCESS;
}
