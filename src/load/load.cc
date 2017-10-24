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
#include "flint/ls.h"
#include "filter/writer.h"
#include "layout.h"
#include "lo/layout.h"
#include "load/param.h"
#include "load/var.h"
#include "phml.h"
#include "phz.h"
#include "runtime.h"
#include "sbml.h"
#include "task.h"
#include "ts.h"

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

class Loader {
public:
	explicit Loader(const boost::filesystem::path &dir)
		: dir_(dir.empty() ? "." : dir)
		, layout_(dir_)
		, model_(dir_)
		, modeldb_(dir_)
		, nc_(dir_)
		, param_(dir_)
		, phz_(dir_)
		, unitoftime_(dir_)
		, var_(dir_)
	{
		layout_ /= "layout";
		model_ /= "model";
		modeldb_ /= "model.db";
		nc_ /= "nc";
		param_ /= "param";
		phz_ /= "phz";
		unitoftime_ /= "unitoftime";
		var_ /= "var";
	}

	const boost::filesystem::path &model() const {return model_;}
	const boost::filesystem::path &modeldb() const {return modeldb_;}
	const boost::filesystem::path &param() const {return param_;}
	const boost::filesystem::path &phz() const {return phz_;}
	const boost::filesystem::path &var() const {return var_;}

	task::Task *LoadCellml(sqlite3 *db, std::vector<double> *data)
	{
		if (!cellml::Read(db))
			return nullptr;
		if (!layout::Generate(db, layout_))
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
		std::unique_ptr<task::Task> task(new task::Task);
		if (!LoadFlows(db, &task->inbound))
			return nullptr;
		if (!runtime::Init(db, 0, layout_, init_bc.get(), &task->inbound, nullptr, data))
			return nullptr;
		return task.release();
	}

	task::Task *LoadPhml(sqlite3 *db, std::vector<double> *data)
	{
		if (!phml::Read(db, dir_))
			return nullptr;
		int seed = static_cast<int>(std::clock());
		if (!phml::Nc(db, nc_, &seed))
			return nullptr;
		if (!phml::UnitOfTime(db, unitoftime_))
			return nullptr;
		if (!phml::LengthAndStep(db, nc_, unitoftime_))
			return nullptr;
		if (!layout::Generate(db, layout_))
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
		if (!LoadFlows(db, &task->inbound))
			return nullptr;
		if (!ts::LoadTimeseriesVector(db, &task->tv))
			return nullptr;
		if (!runtime::Init(db, seed, layout_, init_bc.get(), &task->inbound, &task->tv, data))
			return nullptr;
		return task.release();
	}

	task::Task *LoadSbml(sqlite3 *db, std::vector<double> *data)
	{
		if (!flint::sbml::Read(db))
			return nullptr;
		if (!layout::Generate(db, layout_))
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
		std::unique_ptr<task::Task> task(new task::Task);
		if (!runtime::Init(db, 0, layout_, init_bc.get(), &task->inbound, nullptr, data))
			return nullptr;
		return task.release();
	}

private:
	boost::filesystem::path dir_;
	boost::filesystem::path layout_;
	boost::filesystem::path model_;
	boost::filesystem::path modeldb_;
	boost::filesystem::path nc_;
	boost::filesystem::path param_;
	boost::filesystem::path phz_;
	boost::filesystem::path unitoftime_;
	boost::filesystem::path var_;
};

}

task::Task *Load(const char *given_file, ConfigMode mode,
				 const boost::filesystem::path &dir, std::vector<double> *data)
{
	Loader loader(dir);
	auto driver = db::Driver::Create(loader.modeldb());
	sqlite3 *db = driver->db();
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
