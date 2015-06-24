/* -*- Mode: C++; tab-width: 4; indent-tabs-mode: t; c-basic-offset: 4 -*- vim:set ts=4 sw=4 sts=4 noet: */
#include "task.h"

#include <cstdio>

#include "database.h"
#include "db/driver.h"

namespace workspace {

Task::Task(const char *given_file, int task_id)
	: given_file_(given_file),
	  task_id_(task_id)
{}

bool Task::Setup(file::Format *format)
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
	return file::Txt(given_file_, format, task_id_);
}

}
