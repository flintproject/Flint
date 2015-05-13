/* -*- Mode: C++; tab-width: 4; indent-tabs-mode: t; c-basic-offset: 4 -*- vim:set ts=4 sw=4 sts=4 noet: */
#ifdef HAVE_CONFIG_H
#include "config.h"
#endif

#include <cstdio>
#include <cstdlib>
#include <cstring>
#include <iostream>
#include <string>
#include <vector>

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

bool ParseFile(const char *uuid)
{
	boost::scoped_array<char> db_file(new char[64]); // large enough
	sprintf(db_file.get(), "%s.db", uuid);
	return ParseSbml(db_file.get());
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

typedef std::vector<std::string> UuidVector;

void Usage()
{
	cerr << "usage: flint-combineall DB VALUE FUNCTION ODE" << endl;
}

} // namespace

int main(int argc, char *argv[])
{
	static const int kNumOfArgs = 5;

	if (argc == 2) {
		Usage();
		if (strcmp("-h", argv[1]) == 0 || strcmp("--help", argv[1]) == 0) {
			Usage();
			return EXIT_SUCCESS;
		}
		return EXIT_FAILURE;
	}
	if (argc != kNumOfArgs) {
		Usage();
		return EXIT_FAILURE;
	}

	boost::scoped_ptr<db::Driver> driver(new db::Driver(argv[1]));

	UuidVector uv;
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
		uv.push_back((const char *)uuid);
	}
	if (e != SQLITE_DONE) {
		cerr << "failed to step statement: " << e << endl;
		return EXIT_FAILURE;
	}
	sqlite3_finalize(stmt);

	for (UuidVector::const_iterator it=uv.begin();it!=uv.end();++it) {
		if (!ParseFile(it->c_str())) return EXIT_FAILURE;
	}

	for (int i=2;i<kNumOfArgs;i++) {
		if (!TouchFile(argv[i])) return EXIT_FAILURE;
	}

	for (UuidVector::const_iterator it=uv.begin();it!=uv.end();++it) {
		if (!Combine(it->c_str(),
					 argv[1],
					 argv[2],
					 argv[3],
					 argv[4]))
			return EXIT_FAILURE;
	}

	return EXIT_SUCCESS;
}
