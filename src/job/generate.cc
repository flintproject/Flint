/* -*- Mode: C++; tab-width: 4; indent-tabs-mode: t; c-basic-offset: 4 -*- vim:set ts=4 sw=4 sts=4 noet: */
#include "job.h"

#include <cassert>
#include <cstdio>
#include <cstring>
#include <iostream>
#include <memory>
#include <sstream>
#include <string>

#define BOOST_FILESYSTEM_NO_DEPRECATED
#include <boost/filesystem/fstream.hpp>
#include <boost/uuid/uuid.hpp>

#include "db/driver.h"
#include "db/eq-inserter.h"
#include "db/query.h"
#include "db/statement-driver.h"

namespace flint {
namespace job {

namespace {

class NextJob : db::StatementDriver {
public:
	explicit NextJob(sqlite3 *db)
		: db::StatementDriver(db, "SELECT rowid, ps_id FROM jobs WHERE status = 'pending' LIMIT 1")
	{
	}

	/*
	 * Return 1 if a pending job is found, 0 if no more pending one, or -1 otherwise.
	 */
	int Get(int *rowid, int *ps_id)
	{
		int e;
		e = sqlite3_step(stmt());
		if (e == SQLITE_DONE) {
			// no more pending row
			return 0;
		}
		if (e != SQLITE_ROW) {
			std::cerr << "failed to get a next job: " << e << std::endl;
			return -1;
		}
		*rowid = sqlite3_column_int(stmt(), 0);
		*ps_id = sqlite3_column_int(stmt(), 1);
		return 1;
	}
};

class Inserter {
public:
	Inserter(sqlite3 *db, std::ostream &os)
		: inserter_("parameter_eqs", db)
		, os_(os)
	{}

	bool Insert(const char *name, const char *rhs)
	{
		os_ << name << '=' << rhs << std::endl;

		std::ostringstream oss;
		oss << "(eq %" << name << ' ' << rhs << ')';
		std::string math = oss.str();
		return inserter_.Insert(math.c_str());
	}

	bool Insert(const boost::uuids::uuid &uuid, const char *math)
	{
		return inserter_.Insert(uuid, math);
	}

private:
	db::EqInserter inserter_;
	std::ostream &os_;
};

int SaveParameter(void *data, int argc, char **argv, char **names)
{
	Inserter *inserter = static_cast<Inserter *>(data);
	for (int i=0;i<argc;i++) {
		if (!inserter->Insert(names[i], argv[i]))
			return 1;
	}
	return 0;
}

int SaveEquation(void *data, int argc, char **argv, char **names)
{
	Inserter *inserter = static_cast<Inserter *>(data);
	(void)names;
	assert(argc == 2);
	boost::uuids::uuid uuid;
	assert(argv[0]);
	std::memcpy(&uuid, argv[0], uuid.size());
	return (inserter->Insert(uuid, argv[1])) ? 0 : 1;
}

class Generator {
public:
	Generator(sqlite3 *input, sqlite3 *output, std::ostream &os)
		: input_(input)
		, inserter_(output, os)
	{}

	bool Generate(int rowid, int ps_id) {
		char query[1024];
		char *em;
		int e;

		/* print equations */
		std::sprintf(query, "SELECT * FROM parameter_samples WHERE rowid = '%d'", ps_id);
		e = sqlite3_exec(input_, query, SaveParameter, &inserter_, &em);
		if (e != SQLITE_OK) {
			if (e != SQLITE_ABORT)
				std::cerr << "failed to select parameter_samples: " << e
						  << ": " << em << std::endl;
			sqlite3_free(em);
			return false;
		}
		e = sqlite3_exec(input_, "SELECT uuid, body FROM equations", SaveEquation, &inserter_, &em);
		if (e != SQLITE_OK) {
			if (e != SQLITE_ABORT)
				std::cerr << "failed to select equations: " << e << ": " << em << std::endl;
			sqlite3_free(em);
			return false;
		}

		/* mark it generated */
		std::sprintf(query, "UPDATE jobs SET status = 'generated' WHERE rowid = '%d'", rowid);
		e = sqlite3_exec(input_, query, nullptr, nullptr, &em);
		if (e != SQLITE_OK) {
			std::fprintf(stderr, "failed to update jobs: %d: %s\n", e, em);
			sqlite3_free(em);
			return false;
		}

		return true;
	}

private:
	sqlite3 *input_;
	Inserter inserter_;
};

}

bool Generate(sqlite3 *input, const boost::filesystem::path &dir, int *job_id)
{
	int rowid;
	int ps_id;
	{
		NextJob nj(input);
		int r = nj.Get(&rowid, &ps_id);
		if (r == 0) {
			*job_id = 0;
			return true;
		}
		if (r < 0) return false;
	}
	auto path = BuildPath(dir, rowid);
	boost::system::error_code ec;
	boost::filesystem::create_directories(path, ec);
	if (ec) {
		std::cerr << "failed to create directories: " << path
			 << ": " << ec << std::endl;
		return false;
	}
	boost::filesystem::path filename = path / "values.txt.tmp";
	{
		auto driver = db::Driver::Create(path / "generated.db");
		sqlite3 *output = driver->db();
		if (!output)
			return false;
		if (!BeginTransaction(output))
			return false;
		if (!CreateTable(output, "parameter_eqs", "(uuid BLOB, math TEXT)"))
			return false;
		boost::filesystem::ofstream ofs(filename, std::ios::out);
		if (!ofs) {
			std::cerr << "failed to open " << filename << std::endl;;
			return false;
		}
		if (!BeginTransaction(input)) {
			ofs.close();
			return false;
		}
		{
			Generator g(input, output, ofs);
			if (!g.Generate(rowid, ps_id)) {
				ofs.close();
				return false;
			}
		}
		if (!CommitTransaction(input)) {
			ofs.close();
			return false;
		}
		ofs.close();
		if (!CommitTransaction(output))
			return false;
	}

	boost::filesystem::path values_file = path / "values.txt";
	boost::filesystem::rename(filename, values_file, ec);
	if (ec) {
		std::cerr << "failed to rename " << filename
			 << " to " << values_file
			 << std::endl;
		boost::filesystem::remove(filename);
		return false;
	}

	*job_id = rowid;
	return true;
}

}
}
