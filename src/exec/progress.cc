/* -*- Mode: C++; tab-width: 4; indent-tabs-mode: t; c-basic-offset: 4 -*- vim:set ts=4 sw=4 sts=4 noet: */
#ifdef HAVE_CONFIG_H
#include "config.h"
#endif

#include "exec/progress.h"

#include <cassert>
#include <chrono>
#include <cstring>
#include <numeric>
#include <set>

namespace flint {
namespace exec {

bool MonitorTaskProgress(std::vector<std::future<bool> > &v,
						 boost::interprocess::mapped_region *mr)
{
	bool result = true;
	size_t n = v.size();
	assert(n > 0);
	std::vector<size_t> progress(n, 0);
	std::set<size_t> running;
	for (size_t i=0;i<n;i++)
		running.insert(i);
	char *addr = static_cast<char *>(mr->get_address());
	for (;;) {
		std::this_thread::sleep_for(std::chrono::milliseconds(500));

		auto it = running.begin();
		while (it != running.end()) {
			auto i = *it;
			auto &f = v.at(i);
			std::future_status s = f.wait_for(std::chrono::milliseconds(0));
			switch (s) {
			case std::future_status::deferred:
				assert(false);
				return false;
			case std::future_status::timeout:
				progress[i] = addr[i+1]; // 0-based vs 1-based
				++it;
				break;
			case std::future_status::ready:
				progress[i] = addr[i+1]; // 0-based vs 1-based
				if (!f.get())
					result = false;
				it = running.erase(it);
				break;
			}
		}
		char c = static_cast<char>(std::accumulate(progress.begin(), progress.end(), 0)/n);
		assert(0 <= c && c <= 100);
		*addr = c;
		if (running.empty())
			return result;
	}
	assert(false);
	return false;
}

}
}
