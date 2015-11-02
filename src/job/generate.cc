/* -*- Mode: C++; tab-width: 4; indent-tabs-mode: t; c-basic-offset: 4 -*- vim:set ts=4 sw=4 sts=4 noet: */
#include "job.hh"

#include <cassert>
#include <cstdio>
#include <cstring>
#include <iostream>
#include <memory>
#include <sstream>
#include <string>

#define BOOST_FILESYSTEM_NO_DEPRECATED
#include <boost/filesystem.hpp>
#include <boost/uuid/uuid.hpp>

#include "db/driver.hh"
#include "db/eq-inserter.h"
#include "db/query.h"
#include "db/statement-driver.hh"

using std::cerr;
using std::endl;
using std::fclose;
using std::fopen;
using std::fprintf;
using std::memcpy;
using std::perror;
using std::sprintf;

namespace flint {
namespace job {

namespace {

class NextJob : db::StatementDriver {
public:
	explicit NextJob(sqlite3 *db)
		: db::StatementDriver(db, "SELECT rowid, enum_id FROM jobs WHERE status = 'pending' LIMIT 1")
	{
	}

	/*
	 * Return 1 if a pending job is found, 0 if no more pending one, or -1 otherwise.
	 */
	int Get(int *rowid, int *enum_id)
	{
		int e;
		e = sqlite3_step(stmt());
		if (e == SQLITE_DONE) {
			// no more pending row
			return 0;
		}
		if (e != SQLITE_ROW) {
			cerr << "failed to get a next job: " << e << endl;
			return -1;
		}
		*rowid = sqlite3_column_int(stmt(), 0);
		*enum_id = sqlite3_column_int(stmt(), 1);
		return 1;
	}
};

class Inserter {
public:
	Inserter(sqlite3 *db, FILE *fp)
		: inserter_("parameter_eqs", db)
		, fp_(fp)
	{}

	bool Insert(const char *name, const char *rhs)
	{
		fprintf(fp_, "%s=%s\n", name, rhs);

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
	FILE *fp_;
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
	memcpy(&uuid, argv[0], uuid.size());
	return (inserter->Insert(uuid, argv[1])) ? 0 : 1;
}

class Generator {
public:
	Generator(sqlite3 *input, sqlite3 *output, FILE *fp)
		: input_(input)
		, inserter_(output, fp)
	{}

	bool Generate(int rowid, int enum_id) {
		char query[1024];
		char *em;
		int e;

		/* print equations */
		sprintf(query, "SELECT * FROM enum WHERE rowid = '%d'", enum_id);
		e = sqlite3_exec(input_, query, SaveParameter, &inserter_, &em);
		if (e != SQLITE_OK) {
			fprintf(stderr, "failed to select enum: %d: %s\n", e, em);
			sqlite3_free(em);
			return false;
		}
		e = sqlite3_exec(input_, "SELECT uuid, body FROM equations", SaveEquation, &inserter_, &em);
		if (e != SQLITE_OK) {
			fprintf(stderr, "failed to select equations: %d: %s\n", e, em);
			sqlite3_free(em);
			return false;
		}

		/* mark it generated */
		sprintf(query, "UPDATE jobs SET status = 'generated' WHERE rowid = '%d'", rowid);
		e = sqlite3_exec(input_, query, NULL, NULL, &em);
		if (e != SQLITE_OK) {
			fprintf(stderr, "failed to update jobs: %d: %s\n", e, em);
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

bool Generate(sqlite3 *input, const char *dir, int *job_id)
{
	int rowid;
	int enum_id;
	{
		NextJob nj(input);
		int r = nj.Get(&rowid, &enum_id);
		if (r == 0) {
			*job_id = 0;
			return true;
		}
		if (r < 0) return false;
	}
	std::unique_ptr<char[]> path(BuildPath(dir, rowid));
	boost::system::error_code ec;
	boost::filesystem::create_directories(path.get(), ec);
	if (ec) {
		cerr << "failed to create directories: " << path.get()
			 << ": " << ec << endl;
		return false;
	}
	char filename[96];
	sprintf(filename, "%s/generated.db", path.get());
	db::Driver driver(filename);
	sqlite3 *output = driver.db();
	if (!BeginTransaction(output))
		return false;
	if (!CreateTable(output, "parameter_eqs", "(uuid BLOB, math TEXT)"))
		return false;
	sprintf(filename, "%s/values.txt.tmp", path.get());
	FILE *fp = fopen(filename, "w");
	if (!fp) {
		perror(filename);
		return false;
	}
	if (!BeginTransaction(input)) {
		fclose(fp);
		return false;
	}
	Generator g(input, output, fp);
	if (!g.Generate(rowid, enum_id)) {
		fclose(fp);
		return false;
	}
	if (!CommitTransaction(input)) {
		fclose(fp)
		return false;
	}
	fclose(fp);
	char values_file[96]; // large enough
	sprintf(values_file, "%s/values.txt", path.get());
	if (std::rename(filename, values_file) != 0) {
		cerr << "failed to rename " << filename
			 << " to " << values_file
			 << endl;
		std::remove(filename);
		return false;
	}

	if (!CommitTransaction(output))
		return false;
	*job_id = rowid;
	return true;
}

}
}
