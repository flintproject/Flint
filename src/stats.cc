/* -*- Mode: C++; tab-width: 4; indent-tabs-mode: t; c-basic-offset: 4 -*- vim:set ts=4 sw=4 sts=4 noet: */
#ifdef HAVE_CONFIG_H
#include "config.h"
#endif

#include "flint/stats.h"

#include <cstdio>
#include <iostream>
#include <memory>

#define BOOST_FILESYSTEM_NO_DEPRECATED
#include <boost/filesystem/fstream.hpp>

namespace flint {
namespace stats {

bool Record(const std::chrono::time_point<std::chrono::steady_clock> &rt_start,
			int num_steps,
			const boost::filesystem::path &dir)
{
	// Stop watch at first
	auto rt_end = std::chrono::steady_clock::now();
	auto duration = std::chrono::duration_cast<std::chrono::microseconds>(rt_end - rt_start).count();

	if (dir.empty())
		return true; // do nothing if dir is empty
	auto filename = dir / "stats.txt";
	boost::filesystem::ofstream ofs(filename, std::ios::out|std::ios::binary);
	if (!ofs) {
		std::cerr << "failed to open " << filename << std::endl;
		return false;
	}
	ofs << "number of steps: " << num_steps << std::endl;
	ofs << "duration (microseconds): " << duration << std::endl;
	if (num_steps > 0)
		ofs << "mean (microseconds): " << (duration/num_steps) << std::endl;
	ofs.close();
	return true;
}

}
}
