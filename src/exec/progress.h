/* -*- Mode: C++; tab-width: 4; indent-tabs-mode: t; c-basic-offset: 4 -*- vim:set ts=4 sw=4 sts=4 noet: */
#ifndef FLINT_EXEC_PROGRESS_H_
#define FLINT_EXEC_PROGRESS_H_

#include <future>
#include <vector>

#define BOOST_DATE_TIME_NO_LIB
#include <boost/interprocess/file_mapping.hpp>
#include <boost/interprocess/mapped_region.hpp>

namespace flint {
namespace exec {

/*
 * Given the number of possible combinations of parameter values,
 * create a new file with name "progress" of byte-length (n + 1)
 * in the directory dir.
 * Return its file mapping in case of success, null otherwise.
 */
boost::interprocess::file_mapping *CreateProgressFile(int n, const char *dir);

/*
 * Given a vector of futures, report the task's progress at
 * the 0th byte of the file "progress".
 * Return true in case of success, false otherwise.
 */
bool MonitorTaskProgress(std::vector<std::future<bool> > &v,
						 boost::interprocess::mapped_region *mr);

}
}

#endif
