/* -*- Mode: C++; tab-width: 4; indent-tabs-mode: t; c-basic-offset: 4 -*- vim:set ts=4 sw=4 sts=4 noet: */
#ifndef FLINT_EXEC_TASK_RUNNER_H_
#define FLINT_EXEC_TASK_RUNNER_H_

#include <memory>

#include "db/driver.h"
#include "db/read-only-driver.h"
#include "sqlite3.h"

#include <vector>

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
	TaskRunner(int id, char *path);

	~TaskRunner();

	const char *dir() const {return dir_.get();}
	const char *layout() const {return layout_.get();}
	const char *generated_layout() const {return generated_layout_.get();}
	const std::vector<double> &data() const {return data_;}

	task::Task *GetTask();
	sqlite3 *GetDatabase();
	sqlite3 *GetModelDatabase();

	const cas::DimensionAnalyzer *GetDimensionAnalyzer() const;

	bool Run();

private:
	bool CreateControlFile(int num_samples);
	bool CreateProgressFile(int num_samples);
	bool CreateRssFile(int num_samples);

	int id_;
	std::unique_ptr<char[]> path_;
	std::unique_ptr<char[]> dir_;
	std::unique_ptr<char[]> layout_;
	std::unique_ptr<char[]> generated_layout_;
	std::unique_ptr<task::Task> task_;
	std::vector<double> data_;
	std::unique_ptr<db::Driver> db_driver_;
	std::unique_ptr<db::ReadOnlyDriver> modeldb_driver_;
	std::unique_ptr<cas::DimensionAnalyzer> dimension_analyzer_;
};

}
}

#endif
