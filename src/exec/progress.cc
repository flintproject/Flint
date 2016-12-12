/* -*- Mode: C++; tab-width: 4; indent-tabs-mode: t; c-basic-offset: 4 -*- vim:set ts=4 sw=4 sts=4 noet: */
#ifdef HAVE_CONFIG_H
#include "config.h"
#endif

#include "exec/progress.h"

#include <cassert>
#include <chrono>
#include <cstdio>
#include <cstring>
#include <iostream>
#include <memory>
#include <numeric>
#include <set>

namespace flint {
namespace exec {

boost::interprocess::file_mapping *CreateProgressFile(int n, const char *dir)
{
	size_t len = std::strlen(dir);
	std::unique_ptr<char[]> filename(new char[len + 32]); // large enough
	std::sprintf(filename.get(), "%s/progress.tmp", dir);
	FILE *fp = std::fopen(filename.get(), "wb");
	if (!fp) {
		std::perror(filename.get());
		return nullptr;
	}
	int r = std::fseek(fp, n, SEEK_SET);
	if (r != 0) {
		std::cerr << "failed to seek: " << filename.get() << std::endl;
		std::fclose(fp);
		return nullptr;
	}
	if (std::fputc('\0', fp) == EOF) {
		std::cerr << "failed to write null character: " << filename.get() << std::endl;
		std::fclose(fp);
		return nullptr;
	}
	std::fclose(fp);

	// rename progress.tmp to progress
	std::unique_ptr<char[]> progress(new char[len + 32]); // large enough
	std::sprintf(progress.get(), "%s/progress", dir);
	if (std::rename(filename.get(), progress.get()) != 0) {
		std::cerr << "failed to rename " << filename.get()
			 << " to " << progress.get()
			 << std::endl;
		std::remove(filename.get());
		return nullptr;
	}
	return new boost::interprocess::file_mapping(progress.get(),
												 boost::interprocess::read_write);
}

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
