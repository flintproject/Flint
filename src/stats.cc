/* -*- Mode: C++; tab-width: 4; indent-tabs-mode: t; c-basic-offset: 4 -*- vim:set ts=4 sw=4 sts=4 noet: */
#ifdef HAVE_CONFIG_H
#include "config.h"
#endif

#include "flint/stats.h"

#include <cinttypes>
#include <cstdio>
#include <memory>

namespace flint {
namespace stats {

bool Record(const std::chrono::time_point<std::chrono::steady_clock> &rt_start,
			int num_steps,
			const std::string &dir)
{
	// Stop watch at first
	auto rt_end = std::chrono::steady_clock::now();
	auto duration = std::chrono::duration_cast<std::chrono::microseconds>(rt_end - rt_start).count();

	size_t len = dir.size();
	if (len == 0)
		return true; // do nothing if dir is empty
	std::unique_ptr<char[]> filename(new char[len+6+1]);
	std::sprintf(filename.get(), "%s/stats.txt", dir.c_str());
	FILE *fp = std::fopen(filename.get(), "w");
	if (!fp) {
		std::perror(filename.get());
		return false;
	}
	std::fprintf(fp, "number of steps: %d\n", num_steps);
	std::fprintf(fp, "duration (microseconds): %" PRId64 "\n", duration);
	if (num_steps > 0)
		std::fprintf(fp, "mean (microseconds): %" PRId64 "\n", duration/num_steps);
	std::fclose(fp);
	return false;
}

}
}
