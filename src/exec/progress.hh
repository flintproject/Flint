/* -*- Mode: C++; tab-width: 4; indent-tabs-mode: t; c-basic-offset: 4 -*- vim:set ts=4 sw=4 sts=4 noet: */
#ifndef FLINT_EXEC_PROGRESS_HH_
#define FLINT_EXEC_PROGRESS_HH_

#include <atomic>
#include <thread>

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
 * Given the same n as above and the resulting mapped_region,
 * create a new thread for summarizing the task's progress at
 * the 0th byte of the file "progress".
 * Return the thread in case of success, null otherwise.
 */
std::thread *CreateTaskProgressThread(size_t n,
									  boost::interprocess::mapped_region *mr,
									  std::atomic<size_t> *done);

}
}

#endif
