/* -*- Mode: C++; tab-width: 4; indent-tabs-mode: t; c-basic-offset: 4 -*- vim:set ts=4 sw=4 sts=4 noet: */
#ifdef HAVE_CONFIG_H
#include "config.h"
#endif

#include "task.h"

#include <cstdlib>
#include <cstring>
#include <sstream>

#include "flint/utf8path.h"

namespace flint {
namespace task {

bool Config(int id, sqlite3 *db, const boost::filesystem::path &dir)
{
	char *em;
	int e;

	std::ostringstream oss;
	{
		std::unique_ptr<char[]> dir_u(GetUtf8FromPath(dir));
		oss << "ATTACH DATABASE '"
			<< dir_u.get()
			<< "/model.db' AS m"
			<< id;
	}
	e = sqlite3_exec(db, oss.str().c_str(), nullptr, nullptr, &em);
	if (e != SQLITE_OK) {
		std::cerr << "failed to attach database: " << e
			 << ": " << em << std::endl;
		return false;
	}

	char query[1028]; // long enough
	std::sprintf(query, "DELETE FROM m%d.config", id);
	e = sqlite3_exec(db, query, nullptr, nullptr, &em);
	if (e != SQLITE_OK) {
		std::cerr << "failed to delete from config: " << e
			 << ": " << em << std::endl;
		return false;
	}

	std::sprintf(query,
			"INSERT INTO m%d.config"
			" SELECT sims.algorithm, sims.length, sims.step, sims.granularity, sims.output_start_time, sims.dps_path"
			" FROM tasks LEFT JOIN sims ON tasks.sim_id = sims.rowid"
			" WHERE tasks.rowid = '%d'",
			id, id);
	e = sqlite3_exec(db, query, nullptr, nullptr, &em);
	if (e != SQLITE_OK) {
		std::cerr << "failed to insert into config: " << e
			 << ": " << em << std::endl;
		return false;
	}

	std::sprintf(query, "DETACH DATABASE m%d", id);
	e = sqlite3_exec(db, query, nullptr, nullptr, &em);
	if (e != SQLITE_OK) {
		std::cerr << "failed to detach database: " << e
			 << ": " << em << std::endl;
		return false;
	}

	return true;
}

}
}
