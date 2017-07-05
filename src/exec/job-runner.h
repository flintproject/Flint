/* -*- Mode: C++; tab-width: 4; indent-tabs-mode: t; c-basic-offset: 4 -*- vim:set ts=4 sw=4 sts=4 noet: */
#ifndef FLINT_EXEC_JOB_RUNNER_H_
#define FLINT_EXEC_JOB_RUNNER_H_

#define BOOST_FILESYSTEM_NO_DEPRECATED
#include <boost/filesystem.hpp>

namespace flint {
namespace exec {

class TaskRunner;

class JobRunner {
public:
	JobRunner(TaskRunner *tr, int id);

	bool Run();

private:
	TaskRunner *tr_;
	int id_;
	boost::filesystem::path dir_;
	boost::filesystem::path isd_;
};

}
}

#endif
