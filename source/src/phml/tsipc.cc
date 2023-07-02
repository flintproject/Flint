/* -*- Mode: C++; tab-width: 4; indent-tabs-mode: t; c-basic-offset: 4 -*- vim:set ts=4 sw=4 sts=4 noet: */
#ifdef HAVE_CONFIG_H
#include "config.h"
#endif

#include "phml/tsipc.h"

#include <cassert>
#include <cstdio>
#include <cstdlib>
#include <cstring>
#include <iostream>
#include <memory>

#include <boost/uuid/uuid.hpp>

#include "db/eq-inserter.h"
#include "db/query.h"
#include "db/statement-driver.h"
#include "fppp.h"

namespace flint {
namespace phml {

namespace {

class Formulator {
public:
	explicit Formulator(sqlite3 *db)
		: driver_(db, "INSERT INTO channels VALUES (?, ?)")
		, tsipcforms_("tsipcforms", db)
		, i_(0)
	{}

	bool operator()(const boost::uuids::uuid &mu,
					const boost::uuids::uuid &tu,
					const char *element_id,
					const char *name)
	{
		int e;
		e = sqlite3_bind_blob(driver_.stmt(), 1, &tu, tu.size(), SQLITE_STATIC);
		if (e != SQLITE_OK) {
			std::cerr << "failed to bind uuid: " << e << std::endl;
			return false;
		}
		e = sqlite3_bind_text(driver_.stmt(), 2, element_id, -1, SQLITE_STATIC);
		if (e != SQLITE_OK) {
			std::cerr << "failed to bind name: " << e << std::endl;
			return false;
		}
		e = sqlite3_step(driver_.stmt());
		if (e != SQLITE_DONE) {
			std::cerr << "failed to step statement: " << e << std::endl;
			return false;
		}
		sqlite3_reset(driver_.stmt());
		std::sprintf(buf_, "(eq %%%s ($Data %d %%time))", name, i_++);
		return tsipcforms_.Insert(mu, buf_);
	}

private:
	db::StatementDriver driver_;
	db::EqInserter tsipcforms_;
	int i_;
	char buf_[64]; // sufficiently large
};

int Process(void *data, int argc, char **argv, char **names)
{
	(void)names;
	assert(argc == 4);
	auto *f = static_cast<Formulator *>(data);
	boost::uuids::uuid mu;
	std::memcpy(&mu, argv[0], mu.size());
	boost::uuids::uuid tu = fppp::GetUuidFromUrl(argv[1]);
	if (std::strlen(argv[2]) > 32) {
		std::cerr << "too long element_id: " << argv[2] << std::endl;
		return 1;
	}
	return (*f)(mu, tu, argv[2], argv[3]) ? 0 : 1;
}

}

bool Tsipc(sqlite3 *db)
{
	if (!BeginTransaction(db))
		return false;
	std::unique_ptr<Formulator> f(new Formulator(db));
	char *em;
	int e = sqlite3_exec(db,
						 "SELECT m.module_id, t.url, r.element_id, p.name FROM tsipc AS t"
						 " LEFT JOIN modules AS m ON t.module_rowid = m.rowid"
						 " LEFT JOIN pqs AS p ON t.module_rowid = p.module_rowid"
						 " LEFT JOIN refts AS r ON p.rowid = r.pq_rowid"
						 " WHERE t.timeseries_id = r.timeseries_id",
						 &Process,
						 f.get(),
						 &em);
	if (e != SQLITE_OK) {
		std::cerr << "failed to select tsipc: " << e << ": " << em << std::endl;
		sqlite3_free(em);
		return false;
	}
	return CommitTransaction(db);
}

}
}
