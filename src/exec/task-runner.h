/* -*- Mode: C++; tab-width: 4; indent-tabs-mode: t; c-basic-offset: 4 -*- vim:set ts=4 sw=4 sts=4 noet: */
#ifndef FLINT_EXEC_TASK_RUNNER_H_
#define FLINT_EXEC_TASK_RUNNER_H_

#include "db/driver.h"
#include "db/read-only-driver.h"
#include "flint/ctrl.h"
#include "sqlite3.h"

#include <memory>
#include <vector>

#define BOOST_FILESYSTEM_NO_DEPRECATED
#include <boost/filesystem.hpp>

namespace flint {

namespace cas {
class DimensionAnalyzer;
}

namespace task {
struct Task;
}

namespace exec {

class TaskRunner {
public:
	TaskRunner(int id, char *path, const boost::filesystem::path &dir,
			   ctrl::Argument *arg);

	~TaskRunner();

	ctrl::Argument *arg() {return arg_;}
	const boost::filesystem::path &dir() const {return dir_;}
	const boost::filesystem::path &layout() const {return layout_;}
	const boost::filesystem::path &generated_layout() const {return generated_layout_;}
	const std::vector<double> &data() const {return data_;}

	task::Task *GetTask();
	sqlite3 *GetDatabase();
	sqlite3 *GetModelDatabase();

	const cas::DimensionAnalyzer *GetDimensionAnalyzer() const;

	bool Run();

private:
	bool CreateSpec(int id, sqlite3 *db);
	bool CreateControlFile(int num_samples);
	bool CreateProgressFile(int num_samples);
	bool CreateRssFile(int num_samples);

	int id_;
	std::unique_ptr<char[]> path_;
	ctrl::Argument *arg_;
	boost::filesystem::path dir_;
	boost::filesystem::path layout_;
	boost::filesystem::path generated_layout_;
	std::unique_ptr<task::Task> task_;
	std::vector<double> data_;
	std::unique_ptr<db::Driver> db_driver_;
	std::unique_ptr<db::ReadOnlyDriver> modeldb_driver_;
	std::unique_ptr<cas::DimensionAnalyzer> dimension_analyzer_;
};

}
}

#endif
