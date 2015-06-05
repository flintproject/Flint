/* -*- Mode: C++; tab-width: 4; indent-tabs-mode: t; c-basic-offset: 4 -*- vim:set ts=4 sw=4 sts=4 noet: */
#include "task.h"

#include <cstdio>
#define BOOST_FILESYSTEM_NO_DEPRECATED
#include <boost/filesystem.hpp>

#include "database.h"
#include "db/driver.h"
#include "file.hh"

namespace workspace {

Task::Task(const char *given_file, int task_id)
	: given_file_(given_file),
	  task_id_(task_id)
{}

bool Task::Setup()
{
	char db_file[32]; // FIXME
	// create the database
	if (task_id_) {
		std::sprintf(db_file, "%d/model", task_id_);
	} else {
		std::sprintf(db_file, "model");
	}
	db::Driver driver(db_file);
	if (!SaveGivenFile(driver.db(), given_file_))
		return false;
	if (task_id_) {
		char dir[32]; // FIXME
		std::sprintf(dir, "%d", task_id_);
		boost::filesystem::path dir_path(dir);
		// create working directories if it does not exist
		if ( !boost::filesystem::is_directory(dir_path) &&
			 !boost::filesystem::create_directory(dir_path) )
			return false;
	}
	return file::Txt(given_file_, task_id_);
}

}
