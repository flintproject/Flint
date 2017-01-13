/* -*- Mode: C++; tab-width: 4; indent-tabs-mode: t; c-basic-offset: 4 -*- vim:set ts=4 sw=4 sts=4 noet: */
#ifdef HAVE_CONFIG_H
#include "config.h"
#endif

#include "load.h"

#include <cstdio>
#include <cstring>
#include <ctime>
#include <iostream>
#include <memory>
#include <string>
#include <vector>

#include "cas/dimension.h"
#include "cellml.h"
#include "compiler.h"
#include "database.h"
#include "db/driver.h"
#include "db/statement-driver.h"
#include "file.h"
#include "layout.h"
#include "load/param.h"
#include "load/var.h"
#include "phml.h"
#include "phz.h"
#include "runtime.h"
#include "sbml.h"
#include "task.h"

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
			std::cerr << "failed to bind method: " << e << std::endl;
			return false;
		}
		e = sqlite3_bind_text(stmt(), 2, length, -1, SQLITE_STATIC);
		if (e != SQLITE_OK) {
			std::cerr << "failed to bind length: " << e << std::endl;
			return false;
		}
		e = sqlite3_bind_text(stmt(), 3, step, -1, SQLITE_STATIC);
		if (e != SQLITE_OK) {
			std::cerr << "failed to bind step: " << e << std::endl;
			return false;
		}
		e = sqlite3_step(stmt());
		if (e != SQLITE_DONE) {
			std::cerr << "failed to step statement: " << e << std::endl;
			return false;
		}
		sqlite3_reset(stmt());
		return true;
	}
};

namespace {

const int kFilenameLength = 64;

}

class Loader {
public:
	explicit Loader(int dir)
		: dir_(new char[kFilenameLength])
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
			std::sprintf(dir_.get(), "%d", dir);
		} else {
			std::sprintf(dir_.get(), ".");
		}
		std::sprintf(layout_.get(), "%s/layout", dir_.get());
		std::sprintf(model_.get(), "%s/model", dir_.get());
		std::sprintf(modeldb_.get(), "%s/model.db", dir_.get());
		std::sprintf(nc_.get(), "%s/nc", dir_.get());
		std::sprintf(param_.get(), "%s/param", dir_.get());
		std::sprintf(phz_.get(), "%s/phz", dir_.get());
		std::sprintf(unitoftime_.get(), "%s/unitoftime", dir_.get());
		std::sprintf(var_.get(), "%s/var", dir_.get());
	}

	const char *model() const {return model_.get();}
	const char *modeldb() const {return modeldb_.get();}
	const char *param() const {return param_.get();}
	const char *phz() const {return phz_.get();}
	const char *var() const {return var_.get();}

	task::Task *LoadCellml(sqlite3 *db, std::vector<double> *data)
	{
		if (!cellml::Read(db))
			return nullptr;
		if (!layout::Generate(db, layout_.get()))
			return nullptr;
		std::unique_ptr<Bytecode> init_bc;
		{
			cas::DimensionAnalyzer da;
			if (!da.Load(db))
				return nullptr;
			compiler::Compiler c(&da);
			init_bc.reset(c.Compile(db, "input_ivs", compiler::Method::kAssign));
			if (!init_bc)
				return nullptr;
		}
		if (!runtime::Init(db, 0, layout_.get(), init_bc.get(), data))
			return nullptr;
		return new task::Task;
	}

	task::Task *LoadPhml(sqlite3 *db, std::vector<double> *data)
	{
		if (!phml::Read(db))
			return nullptr;
		int seed = static_cast<int>(std::clock());
		if (!phml::Nc(db, nc_.get(), &seed))
			return nullptr;
		if (!phml::UnitOfTime(db, unitoftime_.get()))
			return nullptr;
		if (!phml::LengthAndStep(db, nc_.get(), unitoftime_.get()))
			return nullptr;
		if (!layout::Generate(db, layout_.get()))
			return nullptr;
		std::unique_ptr<Bytecode> init_bc;
		std::unique_ptr<task::Task> task(new task::Task);
		{
			cas::DimensionAnalyzer da;
			if (!da.Load(db))
				return nullptr;
			compiler::Compiler c(&da);
			init_bc.reset(c.Compile(db, "input_ivs", compiler::Method::kAssign));
			if (!init_bc)
				return nullptr;
			task->post_bc.reset(c.Compile(db, "after_eqs", compiler::Method::kEvent));
			if (!task->post_bc)
				return nullptr;
			task->pre_bc.reset(c.Compile(db, "before_eqs", compiler::Method::kEvent));
			if (!task->pre_bc)
				return nullptr;
		}
		if (!runtime::Init(db, seed, layout_.get(), init_bc.get(), data))
			return nullptr;
		return task.release();
	}

	task::Task *LoadSbml(sqlite3 *db, std::vector<double> *data)
	{
		if (!flint::sbml::Read(db))
			return nullptr;
		if (!layout::Generate(db, layout_.get()))
			return nullptr;
		std::unique_ptr<Bytecode> init_bc;
		{
			cas::DimensionAnalyzer da;
			if (!da.Load(db))
				return nullptr;
			compiler::Compiler c(&da);
			init_bc.reset(c.Compile(db, "input_ivs", compiler::Method::kAssign));
			if (!init_bc)
				return nullptr;
		}
		if (!runtime::Init(db, 0, layout_.get(), init_bc.get(), data))
			return nullptr;
		return new task::Task;
	}

private:
	std::unique_ptr<char[]> dir_;
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

task::Task *Load(const char *given_file, ConfigMode mode, int dir, std::vector<double> *data)
{
	Loader loader(dir);
	db::Driver driver(loader.modeldb());
	sqlite3 *db = driver.db();
	if (!db)
		return nullptr;
	if (!SaveGivenFile(db, given_file))
		return nullptr;
	file::Format format;
	if (!file::Txt(given_file, &format, dir))
		return nullptr;
	std::unique_ptr<task::Task> task;
	switch (format) {
	case file::kIsml:
	case file::kPhml:
		task.reset(loader.LoadPhml(db, data));
		if (!task)
			return nullptr;
		break;
	case file::kPhz:
		if (!phz::Read(db, loader.phz()))
			return nullptr;
		task.reset(loader.LoadPhml(db, data));
		if (!task)
			return nullptr;
		break;
	case file::kCellml:
		task.reset(loader.LoadCellml(db, data));
		if (!task)
			return nullptr;
		if (mode == kRun) {
			ConfigWriter writer(db);
			if (!writer.Write("rk4", "2000", "0.01"))
				return nullptr;
		}
		break;
	case file::kSbml:
		task.reset(loader.LoadSbml(db, data));
		if (!task)
			return nullptr;
		if (mode == kRun) {
			ConfigWriter writer(db);
			if (!writer.Write("rk4", "100", "0.01"))
				return nullptr;
		}
		break;
	default:
		std::cerr << "unexpected file format: " << format << std::endl;
		return nullptr;
	}
	if (mode == kOpen) {
		if (!Param(db, loader.param()))
			return nullptr;
		if (!Var(db, loader.var()))
			return nullptr;
	}
	return task.release();
}

}
}
