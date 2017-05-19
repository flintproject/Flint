/* -*- Mode: C++; tab-width: 4; indent-tabs-mode: t; c-basic-offset: 4 -*- vim:set ts=4 sw=4 sts=4 noet: */
#ifndef FLINT_STATS_H_
#define FLINT_STATS_H_

#include <chrono>
#include <string>

namespace flint {
namespace stats {

/*
 * Write stats information into file "stats" in given directory.
 */
bool Record(const std::chrono::time_point<std::chrono::steady_clock> &rt_start,
			int num_steps,
			const std::string &dir);

}
}

#endif
