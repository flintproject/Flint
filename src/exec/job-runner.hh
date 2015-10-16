/* -*- Mode: C++; tab-width: 4; indent-tabs-mode: t; c-basic-offset: 4 -*- vim:set ts=4 sw=4 sts=4 noet: */
#ifndef FLINT_EXEC_JOB_RUNNER_HH_
#define FLINT_EXEC_JOB_RUNNER_HH_

#include <memory>

namespace flint {
namespace exec {

class TaskRunner;

class JobRunner {
public:
	JobRunner(TaskRunner *tr, int id);

	bool Run();

private:
	TaskRunner *tr_;
	void *progress_address_;
	std::unique_ptr<char[]> dir_;
	std::unique_ptr<char[]> generated_bc_;
	std::unique_ptr<char[]> generated_db_;
	std::unique_ptr<char[]> generated_init_;
	std::unique_ptr<char[]> stored_;
	std::unique_ptr<char[]> isd_;
};

}
}

#endif
