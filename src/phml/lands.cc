/* -*- Mode: C++; tab-width: 4; indent-tabs-mode: t; c-basic-offset: 4 -*- vim:set ts=4 sw=4 sts=4 noet: */
#ifdef HAVE_CONFIG_H
#include "config.h"
#endif

#include "phml.hh"

#include <cassert>
#include <cstdio>
#include <cstdlib>
#include <fstream>
#include <iostream>
#include <memory>
#include <unordered_map>

#include <boost/rational.hpp>

#include "db/statement-driver.hh"
#include "ipc.pb.h"
#include "phml.pb.h"
#include "bc/pack.h"

using std::cerr;
using std::endl;
using std::fprintf;

namespace flint {
namespace phml {

namespace {

class UnitOfTimeLoader {
public:
	UnitOfTimeLoader(const UnitOfTimeLoader &) = delete;
	UnitOfTimeLoader &operator=(const UnitOfTimeLoader &) = delete;

	explicit UnitOfTimeLoader(const char *file) : ifs_(file, std::ios::in|std::ios::binary) {}

	~UnitOfTimeLoader() {
		if (ifs_.is_open()) ifs_.close();
	}

	template<typename THandler>
	bool Load(THandler *handler) {
		if (!ifs_.is_open()) {
			cerr << "failed to open unitoftime file" << endl;
			return false;
		}
		while (ifs_.peek() != EOF) {
			std::unique_ptr<ipc::TimeUnit> tu(new ipc::TimeUnit);
			if (!UnpackFromIstream(*tu, &ifs_)) {
				cerr << "failed to read TimeUnit" << endl;
				return false;
			}
			handler->AddTimeUnit(std::move(tu));
		}
		return true;
	}

private:
	std::ifstream ifs_;
};

typedef std::unordered_map<int, std::unique_ptr<ipc::TimeUnit> > TimeUnitMap;

class UnitOfTimeHandler {
public:
	UnitOfTimeHandler(const UnitOfTimeHandler &) = delete;
	UnitOfTimeHandler &operator=(const UnitOfTimeHandler &) = delete;

	explicit UnitOfTimeHandler(TimeUnitMap *tum) : tum_(tum) {}

	void AddTimeUnit(std::unique_ptr<ipc::TimeUnit> &&tu) {
		int id = tu->id();
		tum_->insert(std::make_pair(id, std::move(tu)));
	}

private:
	TimeUnitMap *tum_;
};

class NumericalConfigurationLoader {
public:
	NumericalConfigurationLoader(const NumericalConfigurationLoader &) = delete;
	NumericalConfigurationLoader &operator=(const NumericalConfigurationLoader &) = delete;

	explicit NumericalConfigurationLoader(const char *file) : ifs_(file, std::ios::in|std::ios::binary) {}

	~NumericalConfigurationLoader() {
		if (ifs_.is_open()) ifs_.close();
	}

	bool Load(::phml::NumericalConfiguration *nc) {
		if (!ifs_.is_open()) {
			cerr << "failed to open nc file" << endl;
			return false;
		}
		return nc->ParseFromIstream(&ifs_);
	}

private:
	std::ifstream ifs_;
};

class LengthAndStepWriter : db::StatementDriver {
public:
	explicit LengthAndStepWriter(sqlite3 *db)
		: db::StatementDriver(db, "UPDATE config SET length = ?, step = ?")
	{}

	bool Write(double length, const char *step) {
		int e;
		e = sqlite3_bind_double(stmt(), 1, length);
		if (e != SQLITE_OK) {
			cerr << "failed to bind length: " << e << endl;
			return false;
		}
		e = sqlite3_bind_text(stmt(), 2, step, -1, SQLITE_STATIC);
		if (e != SQLITE_OK) {
			cerr << "failed to bind step: " << e << endl;
			return false;
		}
		e = sqlite3_step(stmt());
		if (e != SQLITE_DONE) {
			cerr << "failed to step statement: " << e << endl;
			return false;
		}
		sqlite3_reset(stmt());
		return true;
	}
};

}

bool LengthAndStep(sqlite3 *db, const char *nc_file, const char *uot_file)
{
	::phml::NumericalConfiguration nc;
	{
		std::unique_ptr<NumericalConfigurationLoader> loader(new NumericalConfigurationLoader(nc_file));
		if (!loader->Load(&nc)) return false;
	}

	TimeUnitMap tum;
	{
		std::unique_ptr<UnitOfTimeHandler> handler(new UnitOfTimeHandler(&tum));
		std::unique_ptr<UnitOfTimeLoader> loader(new UnitOfTimeLoader(uot_file));
		if (!loader->Load(handler.get())) return false;
	}

	LengthAndStepWriter writer(db);
	TimeUnitMap::const_iterator it;
	if (nc.has_sts()) {
		if (nc.has_td()) {
			int sts_unit_id = nc.sts().unit_id();
			it = tum.find(sts_unit_id);
			if (it == tum.end()) {
				cerr << "unknown unit-id of simulation-time-span: " << sts_unit_id << endl;
				return false;
			}
			const std::unique_ptr<ipc::TimeUnit> &sts_unit = it->second;
			boost::rational<long> r_sts(sts_unit->n(), sts_unit->d());

			int td_unit_id = nc.td().unit_id();
			it = tum.find(td_unit_id);
			if (it == tum.end()) {
				cerr << "unknown unit-id of time-discretization: " << td_unit_id << endl;
				return false;
			}
			const std::unique_ptr<ipc::TimeUnit> &td_unit = it->second;
			boost::rational<long> r_td(td_unit->n(), td_unit->d());

			r_sts /= r_td;

			double len;
			if (std::sscanf(nc.sts().value().c_str(), "%lf", &len) != 1) {
				cerr << "invalid value of simulation-time-span: " << nc.sts().value() << endl;
				return false;
			}
			len *= boost::rational_cast<double>(r_sts);
			if (!writer.Write(len, nc.td().step().c_str()))
				return false;
		} else {
			double len = std::strtod(nc.sts().value().c_str(), NULL);
			// TODO: check len
			if (!writer.Write(len, "0.01"))
				return false;
		}
	} else if (nc.has_td()) {
		if (!writer.Write(100, nc.td().step().c_str()))
			return false;
	} else {
		if (!writer.Write(100, "0.01"))
			return false;
	}
	return true;
}

}
}
