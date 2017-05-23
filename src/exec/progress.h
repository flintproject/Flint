/* -*- Mode: C++; tab-width: 4; indent-tabs-mode: t; c-basic-offset: 4 -*- vim:set ts=4 sw=4 sts=4 noet: */
#ifndef FLINT_EXEC_PROGRESS_H_
#define FLINT_EXEC_PROGRESS_H_

#include <future>
#include <vector>

#define BOOST_DATE_TIME_NO_LIB
#include <boost/interprocess/mapped_region.hpp>

namespace flint {
namespace exec {

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
