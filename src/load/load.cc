/* -*- Mode: C++; tab-width: 4; indent-tabs-mode: t; c-basic-offset: 4 -*- vim:set ts=4 sw=4 sts=4 noet: */
#ifdef HAVE_CONFIG_H
#include "config.h"
#endif

#include "load.hh"

#define BOOST_FILESYSTEM_NO_DEPRECATED
#include <boost/filesystem.hpp>

#include <cstdio>
#include <cstring>
#include <iostream>
#include <string>
#include <vector>

#include "cellml.hh"
#include "compiler.hh"
#include "db/driver.h"
#include "db/statement-driver.h"
#include "layout.hh"
#include "load/param.hh"
#include "load/var.hh"
#include "phml.hh"
#include "phz.hh"
#include "runtime.hh"
#include "sbml.hh"

using std::cerr;
using std::endl;
using std::fclose;
using std::fopen;
using std::fprintf;
using std::perror;

namespace load {

namespace {

class ConfigWriter : db::StatementDriver {
public:
	explicit ConfigWriter(sqlite3 *db)
		: db::StatementDriver(db, "UPDATE config SET method = ?, length = ?, step = ?")
	{}

	bool Write(const char *method, const char *length, const char *step) {
		int e;
		e = sqlite3_bind_text(stmt(), 1, method, -1, SQLITE_STATIC);
		if (e != SQLITE_OK) {
			cerr << "failed to bind method: " << e << endl;
			return false;
		}
		e = sqlite3_bind_text(stmt(), 2, length, -1, SQLITE_STATIC);
		if (e != SQLITE_OK) {
			cerr << "failed to bind length: " << e << endl;
			return false;
		}
		e = sqlite3_bind_text(stmt(), 3, step, -1, SQLITE_STATIC);
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

bool CopyFile(const char *source, const char *target)
{
	boost::system::error_code ec;
	boost::filesystem::copy_file(source, target, ec);
	if (ec) {
		cerr << ec << endl;
		return false;
	}
	return true;
}

bool LoadCellml(ConfigMode mode, sqlite3 *db)
{
	if (!cellml::Read(db))
		return false;
	if (!layout::Generate(db, "layout"))
		return false;
	if (!compiler::Compile(db, "input_ivs", "assign", "const-bc"))
		return false;
	if (!runtime::Init(db, "layout", "const-bc", "init"))
		return false;
	if (mode == kRun) {
		ConfigWriter writer(db);
		if (!writer.Write("rk4", "2000", "0.01"))
			return false;
	}
	return true;
}

bool LoadPhml(sqlite3 *db)
{
	if (!phml::Read(db))
		return false;
	if (!phml::Nc(db, "nc"))
		return false;
	if (!phml::UnitOfTime(db, "unitoftime"))
		return false;
	if (!phml::LengthAndStep(db, "nc", "unitoftime"))
		return false;
	if (!layout::Generate(db, "layout"))
		return false;
	if (!compiler::Compile(db, "input_ivs", "assign", "const-bc"))
		return false;
	if (!compiler::Compile(db, "after_eqs", "event", "after-bc"))
		return false;
	if (!compiler::Compile(db, "before_eqs", "event", "before-bc"))
		return false;
	if (!runtime::Init(db, "layout", "const-bc", "init"))
		return false;
	return true;
}

bool LoadSbml(ConfigMode mode, sqlite3 *db)
{
	if (!flint::sbml::Read(db))
		return false;
	if (!layout::Generate(db, "layout"))
		return false;
	if (!compiler::Compile(db, "input_ivs", "assign", "output-bc"))
		return false;
	if (!runtime::Init(db, "layout", "output-bc", "init"))
		return false;
	if (mode == kRun) {
		ConfigWriter writer(db);
		if (!writer.Write("rk4", "100", "0.01"))
			return false;
	}
	return true;
}

}

bool Load(file::Format format, ConfigMode mode, int dir)
{
	if (dir) {
		char buf[32];
		sprintf(buf, "%d", dir);
		boost::system::error_code ec;
		boost::filesystem::current_path(buf, ec);
		if (ec) {
			cerr << "failed to change directory: " << buf
				 << ": " << ec << endl;
			return false;
		}
	}

	if (!CopyFile("model", "modeldb"))
		return false;
	db::Driver driver("modeldb");
	sqlite3 *db = driver.db();
	switch (format) {
	case file::kIsml:
	case file::kPhml:
		if (!LoadPhml(db))
			return false;
		break;
	case file::kPhz:
		if (!phz::Read(db, "phz"))
			return false;
		if (!LoadPhml(db))
			return false;
		break;
	case file::kCellml:
		if (!LoadCellml(mode, db))
			return false;
		break;
	case file::kSbml:
		if (!LoadSbml(mode, db))
			return false;
		break;
	default:
		cerr << "unexpected file format: " << format << endl;
		return false;
	}
	if (!Param(db, "param"))
		return false;
	if (!Var(db, "var"))
		return false;

	if (dir) {
		boost::system::error_code ec;
		boost::filesystem::current_path("..", ec);
		if (ec) {
			cerr << "failed to change directory to :.. : " << ec << endl;
			return false;
		}
	}

	return true;
}

}
