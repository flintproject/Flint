/* -*- Mode: C++; tab-width: 4; indent-tabs-mode: t; c-basic-offset: 4 -*- vim:set ts=4 sw=4 sts=4 noet: */
#ifndef FLINT_GUI_JOB_H_
#define FLINT_GUI_JOB_H_

#define BOOST_DATE_TIME_NO_LIB
#include <boost/interprocess/mapped_region.hpp>

namespace flint {
namespace gui {

struct Task;

struct Job {
	const Task &task;
	int id;

	Job(const Task &given_task, int given_id);

	boost::interprocess::mapped_region GetProgressRegion() const;
};

}
}

#endif
