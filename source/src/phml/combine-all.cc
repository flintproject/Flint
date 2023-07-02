/* -*- Mode: C++; tab-width: 4; indent-tabs-mode: t; c-basic-offset: 4 -*- vim:set ts=4 sw=4 sts=4 noet: */
#ifdef HAVE_CONFIG_H
#include "config.h"
#endif

#include "phml.h"

#include <cassert>
#include <cstdio>
#include <cstdlib>
#include <cstring>
#include <iostream>
#include <memory>
#include <string>
#include <vector>

#define BOOST_FILESYSTEM_NO_DEPRECATED
#include <boost/filesystem.hpp>
#include <boost/uuid/uuid_io.hpp>

#include "database.h"
#include "db/driver.h"
#include "db/utility.h"
#include "flint/utf8path.h"
#include "phml/combine.h"
#include "phml/import.h"
#include "sbml.h"

namespace flint {
namespace phml {

namespace {

std::unique_ptr<db::Driver> GetDriver(const boost::uuids::uuid &uuid,
									  const boost::filesystem::path &dir)
{
	boost::filesystem::path db_path = dir / boost::uuids::to_string(uuid);
	db_path.replace_extension("db");
	return db::Driver::Create(db_path);
}

bool SaveFile(const boost::uuids::uuid &uuid,
			  const char *xml_file,
			  const boost::filesystem::path &dir)
{
	auto driver = GetDriver(uuid, dir);
	return SaveGivenFile(driver->db(), xml_file);
}

bool ParseFile(const boost::uuids::uuid &uuid,
			   const boost::filesystem::path &dir)
{
	auto driver = GetDriver(uuid, dir);
	return flint::sbml::Parse(driver->db());
}

typedef std::vector<boost::uuids::uuid> UuidVector;

}

bool CombineAll(sqlite3 *db, const boost::filesystem::path &dir)
{
	UuidVector uv;
	sqlite3_stmt *stmt;
	int e = db::PrepareStatement(db,
								 "SELECT m.module_id, i.type, i.ref FROM imports AS i LEFT JOIN modules AS m ON i.module_rowid = m.rowid",
								 &stmt);
	if (e != SQLITE_OK) {
		std::cerr << "failed to prepare statement: " << e << std::endl;
		return false;
	}

	for (e = sqlite3_step(stmt); e == SQLITE_ROW; e = sqlite3_step(stmt)) {
		const void *module_id = sqlite3_column_blob(stmt, 0);
		assert(module_id);
		boost::uuids::uuid uuid;
		std::memcpy(&uuid, module_id, uuid.size());
		const unsigned char *type = sqlite3_column_text(stmt, 1);
		const unsigned char *ref = sqlite3_column_text(stmt, 2);

		if (std::strcmp(reinterpret_cast<const char *>(type), "external") == 0) {
			if (!SaveFile(uuid, reinterpret_cast<const char *>(ref), dir))
				return false;
		} else {
			auto xml_path = DumpImport(db, uuid, dir);
			if (xml_path.empty())
				return false;
			assert(xml_path.is_absolute());
			std::unique_ptr<char[]> xml_utf8(GetUtf8FromPath(xml_path));
			if (!xml_utf8)
				return false;
			if (!SaveFile(uuid, xml_utf8.get(), dir))
				return false;
		}
		uv.push_back(uuid);
	}
	if (e != SQLITE_DONE) {
		std::cerr << "failed to step statement: " << e << std::endl;
		return false;
	}
	sqlite3_finalize(stmt);

	for (const auto &u : uv) {
		if (!ParseFile(u, dir))
			return false;
	}

	for (const auto &u : uv) {
		if (!Combine(u, db, dir))
			return false;
	}

	return true;
}

}
}
