/* -*- Mode: C++; tab-width: 4; indent-tabs-mode: t; c-basic-offset: 4 -*- vim:set ts=4 sw=4 sts=4 noet: */
#ifdef HAVE_CONFIG_H
#include "config.h"
#endif

#include "load.hh"

#include <cstdio>
#include <cstring>
#include <ctime>
#include <iostream>
#include <memory>
#include <string>
#include <vector>

#include "cellml.hh"
#include "compiler.hh"
#include "database.h"
#include "db/driver.hh"
#include "db/statement-driver.hh"
#include "file.hh"
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
using std::sprintf;

namespace flint {
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

class Loader {
public:
	static const int kFilenameLength = 64;

	explicit Loader(int dir)
		: dir_(new char[kFilenameLength])
		, after_bc_(new char[kFilenameLength])
		, before_bc_(new char[kFilenameLength])
		, const_bc_(new char[kFilenameLength])
		, init_(new char[kFilenameLength])
		, layout_(new char[kFilenameLength])
		, model_(new char[kFilenameLength])
		, modeldb_(new char[kFilenameLength])
		, nc_(new char[kFilenameLength])
		, param_(new char[kFilenameLength])
		, phz_(new char[kFilenameLength])
		, unitoftime_(new char[kFilenameLength])
		, var_(new char[kFilenameLength])
	{
		if (dir) {
			sprintf(dir_.get(), "%d", dir);
		} else {
			sprintf(dir_.get(), ".");
		}
		sprintf(after_bc_.get(), "%s/after-bc", dir_.get());
		sprintf(before_bc_.get(), "%s/before-bc", dir_.get());
		sprintf(const_bc_.get(), "%s/const-bc", dir_.get());
		sprintf(init_.get(), "%s/init", dir_.get());
		sprintf(layout_.get(), "%s/layout", dir_.get());
		sprintf(model_.get(), "%s/model", dir_.get());
		sprintf(modeldb_.get(), "%s/model.db", dir_.get());
		sprintf(nc_.get(), "%s/nc", dir_.get());
		sprintf(param_.get(), "%s/param", dir_.get());
		sprintf(phz_.get(), "%s/phz", dir_.get());
		sprintf(unitoftime_.get(), "%s/unitoftime", dir_.get());
		sprintf(var_.get(), "%s/var", dir_.get());
	}

	const char *model() const {return model_.get();}
	const char *modeldb() const {return modeldb_.get();}
	const char *param() const {return param_.get();}
	const char *phz() const {return phz_.get();}
	const char *var() const {return var_.get();}

	bool LoadCellml(sqlite3 *db)
	{
		if (!cellml::Read(db))
			return false;
		if (!layout::Generate(db, layout_.get()))
			return false;
		if (!compiler::Compile(db, "input_ivs", "assign", const_bc_.get()))
			return false;
		return runtime::Init(db, 0, layout_.get(), const_bc_.get(), init_.get());
	}

	bool LoadPhml(sqlite3 *db)
	{
		if (!phml::Read(db))
			return false;
		int seed = static_cast<int>(std::clock());
		if (!phml::Nc(db, nc_.get(), &seed))
			return false;
		if (!phml::UnitOfTime(db, unitoftime_.get()))
			return false;
		if (!phml::LengthAndStep(db, nc_.get(), unitoftime_.get()))
			return false;
		if (!layout::Generate(db, layout_.get()))
			return false;
		if (!compiler::Compile(db, "input_ivs", "assign", const_bc_.get()))
			return false;
		if (!compiler::Compile(db, "after_eqs", "event", after_bc_.get()))
			return false;
		if (!compiler::Compile(db, "before_eqs", "event", before_bc_.get()))
			return false;
		return runtime::Init(db, seed, layout_.get(), const_bc_.get(), init_.get());
	}

	bool LoadSbml(sqlite3 *db)
	{
		if (!flint::sbml::Read(db))
			return false;
		if (!layout::Generate(db, layout_.get()))
			return false;
		if (!compiler::Compile(db, "input_ivs", "assign", const_bc_.get()))
			return false;
		return runtime::Init(db, 0, layout_.get(), const_bc_.get(), init_.get());
	}

private:
	std::unique_ptr<char[]> dir_;
	std::unique_ptr<char[]> after_bc_;
	std::unique_ptr<char[]> before_bc_;
	std::unique_ptr<char[]> const_bc_;
	std::unique_ptr<char[]> init_;
	std::unique_ptr<char[]> layout_;
	std::unique_ptr<char[]> model_;
	std::unique_ptr<char[]> modeldb_;
	std::unique_ptr<char[]> nc_;
	std::unique_ptr<char[]> param_;
	std::unique_ptr<char[]> phz_;
	std::unique_ptr<char[]> unitoftime_;
	std::unique_ptr<char[]> var_;
};

}

bool Load(const char *given_file, ConfigMode mode, int dir)
{
	Loader loader(dir);
	db::Driver driver(loader.modeldb());
	sqlite3 *db = driver.db();
	if (!SaveGivenFile(db, given_file))
		return false;
	file::Format format;
	if (!file::Txt(given_file, &format, dir))
		return false;
	switch (format) {
	case file::kIsml:
	case file::kPhml:
		if (!loader.LoadPhml(db))
			return false;
		break;
	case file::kPhz:
		if (!phz::Read(db, loader.phz()))
			return false;
		if (!loader.LoadPhml(db))
			return false;
		break;
	case file::kCellml:
		if (!loader.LoadCellml(db))
			return false;
		if (mode == kRun) {
			ConfigWriter writer(db);
			if (!writer.Write("rk4", "2000", "0.01"))
				return false;
		}
		break;
	case file::kSbml:
		if (!loader.LoadSbml(db))
			return false;
		if (mode == kRun) {
			ConfigWriter writer(db);
			if (!writer.Write("rk4", "100", "0.01"))
				return false;
		}
		break;
	default:
		cerr << "unexpected file format: " << format << endl;
		return false;
	}
	if (mode == kOpen) {
		if (!Param(db, loader.param()))
			return false;
		if (!Var(db, loader.var()))
			return false;
	}
	return true;
}

}
}
