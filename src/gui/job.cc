/* -*- Mode: C++; tab-width: 4; indent-tabs-mode: t; c-basic-offset: 4 -*- vim:set ts=4 sw=4 sts=4 noet: */
#ifdef HAVE_CONFIG_H
#include "config.h"
#endif

#include "gui/job.h"

#define BOOST_DATE_TIME_NO_LIB
#include <boost/interprocess/file_mapping.hpp>

#include "gui/simulation.h"
#include "gui/task.h"

namespace flint {
namespace gui {

Job::Job(const Task &given_task, int given_id)
	: task(given_task)
	, id(given_id)
{}

boost::interprocess::mapped_region Job::GetProgressRegion() const
{
	auto filename = task.simulation->GetProgressFileName(task.id);
	boost::interprocess::file_mapping fm(filename.GetFullPath().utf8_str().data(),
										 boost::interprocess::read_only);
	return boost::interprocess::mapped_region(fm, boost::interprocess::read_only, 0, id);
}

}
}
