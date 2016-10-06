/* -*- Mode: C++; tab-width: 4; indent-tabs-mode: t; c-basic-offset: 4 -*- vim:set ts=4 sw=4 sts=4 noet: */
#include "flint/background.h"

#include <cstdio>
#include <cstdlib>
#include <cstring>
#include <iostream>
#include <thread>

#define BOOST_FILESYSTEM_NO_DEPRECATED
#include <boost/filesystem.hpp>
#define BOOST_DATE_TIME_NO_LIB
#include <boost/interprocess/sync/file_lock.hpp>
#include <boost/interprocess/sync/scoped_lock.hpp>

#include "utf8path.h"

namespace flint {

namespace {

bool WaitForLockAndDie(const char *filename)
{
	boost::filesystem::path lock_path = GetPathFromUtf8(filename);
	std::string lock_file = lock_path.string();
	boost::interprocess::file_lock lock(lock_file.c_str());
	boost::interprocess::scoped_lock<boost::interprocess::file_lock> sl(lock);
	std::cerr << "the main process seems to have died as the exclusive lock with "
		 << lock_file
		 << " was released."
		 << std::endl;
	std::cerr << "So this simulation process should also be shutdown, bye." << std::endl;
	std::_Exit(EXIT_FAILURE);
}

}

void InitializeBackgroundProcess(const char *filename)
{
	std::thread th(WaitForLockAndDie, filename);
	th.detach();
}

}
