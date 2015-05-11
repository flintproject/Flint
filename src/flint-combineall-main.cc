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
#include "phml/combine.h"
#include "phml/import.h"
#include "sbml/parser.h"
#include "utf8path.h"

#include "system.h"

using std::cerr;
using std::endl;
using std::printf;
using std::sprintf;
using std::strcmp;

namespace {

bool SaveFile(const char *uuid, const char *xml_file)
{
	boost::scoped_array<char> db_file(new char[64]);
	sprintf(db_file.get(), "%s.db", uuid);
	return SaveGivenFile(db_file.get(), xml_file);
}

int ParseFile(void *data, int argc, char **argv, char **names)
{
	(void)data;
	(void)names;
	assert(argc == 1);
	boost::scoped_array<char> db_file(new char[64]); // large enough
	sprintf(db_file.get(), "%s.db", argv[0]);
	if (!ParseSbml(db_file.get())) return 1;
	return 0;
}

int CombineFile(void *data, int argc, char **argv, char **names)
{
	(void)names;
	assert(argc == 1);
	if (!Combine(argv[0],
				 ((char **)data)[1],
				 ((char **)data)[2],
				 ((char **)data)[3],
				 ((char **)data)[4],
				 ((char **)data)[5]))
		return 1;
	return 0;
}

bool TouchFile(const char *file)
{
	FILE *fp = std::fopen(file, "w");
	if (!fp) {
		cerr << "failed to touch " << file << endl;
		return false;
	}
	std::fclose(fp);
	return true;
}

void Usage()
{
	cerr << "usage: flint-combineall DB NAME VALUE FUNCTION ODE" << endl;
}

} // namespace

int main(int argc, char *argv[])
{
	if (argc == 2) {
		Usage();
		if (strcmp("-h", argv[1]) == 0 || strcmp("--help", argv[1]) == 0) {
			Usage();
			return EXIT_SUCCESS;
		}
		return EXIT_FAILURE;
	}
	if (argc != 6) {
		Usage();
		return EXIT_FAILURE;
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

	for (e = sqlite3_step(stmt); e == SQLITE_ROW; e = sqlite3_step(stmt)) {
		const unsigned char *uuid = sqlite3_column_text(stmt, 0);
		const unsigned char *type = sqlite3_column_text(stmt, 1);
		const unsigned char *ref = sqlite3_column_text(stmt, 2);

		if (strcmp((const char *)type, "external") == 0) {
			if (!SaveFile((const char *)uuid, (const char *)ref)) return EXIT_FAILURE;
		} else {
			if (!DumpImport(argv[1], (const char *)uuid)) return EXIT_FAILURE;
			boost::scoped_array<char> xml_file(new char[64]);
			sprintf(xml_file.get(), "%s.xml", uuid);
			boost::filesystem::path xml_path = boost::filesystem::absolute(xml_file.get());
			boost::scoped_array<char> xml_utf8(GetUtf8FromPath(xml_path));
			if (!SaveFile((const char *)uuid, xml_utf8.get())) return EXIT_FAILURE;
		}
	}
	if (e != SQLITE_DONE) {
		cerr << "failed to step statement: " << e << endl;
		return EXIT_FAILURE;
	}
	sqlite3_finalize(stmt);

	char *em;
	e = sqlite3_exec(driver->db(), "SELECT m.module_id FROM imports AS i LEFT JOIN modules AS m ON i.module_rowid = m.rowid",
					 ParseFile, NULL, &em);
	if (e != SQLITE_OK) {
		cerr << e << ": " << em << endl;
		sqlite3_free(em);
		return EXIT_FAILURE;
	}

	const char *name_file = argv[2];
	const char *value_file = argv[3];
	const char *function_file = argv[4];
	const char *ode_file = argv[5];
	if (!TouchFile(name_file)) return EXIT_FAILURE;
	if (!TouchFile(value_file)) return EXIT_FAILURE;
	if (!TouchFile(function_file)) return EXIT_FAILURE;
	if (!TouchFile(ode_file)) return EXIT_FAILURE;

	e = sqlite3_exec(driver->db(), "SELECT m.module_id FROM imports AS i LEFT JOIN modules AS m ON i.module_rowid = m.rowid",
					 CombineFile, argv, &em);
	if (e != SQLITE_OK) {
		cerr << e << ": " << em << endl;
		sqlite3_free(em);
		return EXIT_FAILURE;
	}

	return EXIT_SUCCESS;
}
