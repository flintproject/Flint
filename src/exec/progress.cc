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

using std::fclose;
using std::fopen;
using std::fputc;
using std::fseek;
using std::memcpy;
using std::perror;
using std::remove;
using std::rename;
using std::sprintf;
using std::strlen;

namespace flint {
namespace exec {

boost::interprocess::file_mapping *CreateProgressFile(int n, const char *dir)
{
	size_t len = strlen(dir);
	std::unique_ptr<char[]> filename(new char[len + 32]); // large enough
	sprintf(filename.get(), "%s/progress.tmp", dir);
	FILE *fp = fopen(filename.get(), "wb");
	if (!fp) {
		perror(filename.get());
		return nullptr;
	}
	int r = fseek(fp, n, SEEK_SET);
	if (r != 0) {
		std::cerr << "failed to seek: " << filename.get() << std::endl;
		fclose(fp);
		return nullptr;
	}
	if (fputc('\0', fp) == EOF) {
		std::cerr << "failed to write null character: " << filename.get() << std::endl;
		fclose(fp);
		return nullptr;
	}
	fclose(fp);

	// rename progress.tmp to progress
	std::unique_ptr<char[]> progress(new char[len + 32]); // large enough
	sprintf(progress.get(), "%s/progress", dir);
	if (rename(filename.get(), progress.get()) != 0) {
		std::cerr << "failed to rename " << filename.get()
			 << " to " << progress.get()
			 << std::endl;
		remove(filename.get());
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
			memcpy(addr, &c, 1);
			if (sum == 100)
				return;
		}
	}
}

}

std::thread *CreateTaskProgressThread(size_t n,
									  boost::interprocess::mapped_region *mr,
									  std::atomic<size_t> *done)
{
	return new std::thread(NotifyTaskProgress, n, mr, done);
}

}
}
