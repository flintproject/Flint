/* -*- Mode: C++; tab-width: 4; indent-tabs-mode: t; c-basic-offset: 4 -*- vim:set ts=4 sw=4 sts=4 noet: */
#ifdef HAVE_CONFIG_H
#include "config.h"
#endif

#include "exec/progress.h"

#include <chrono>
#include <cstdint>
#include <cstdio>
#include <cstring>
#include <iostream>
#include <memory>

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

namespace {

void NotifyTaskProgress(size_t n,
						boost::interprocess::mapped_region *mr,
						std::atomic<size_t> *done)
{
	while (n != *done) {
		std::this_thread::sleep_for(std::chrono::milliseconds(250));

		int64_t sum = 0;
		char *addr = static_cast<char *>(mr->get_address());
		for (size_t i=1;i<=n;i++) { // 1-based
			sum += addr[i];
		}
		sum /= n;
		if (0 <= sum && sum <= 100) {
			char c = static_cast<char>(sum);
			std::memcpy(addr, &c, 1);
			if (sum == 100)
				return;
		}
	}
}

}

std::thread CreateTaskProgressThread(size_t n,
									 boost::interprocess::mapped_region *mr,
									 std::atomic<size_t> *done)
{
	return std::thread(NotifyTaskProgress, n, mr, done);
}

}
}
