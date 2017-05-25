/* -*- Mode: C++; tab-width: 4; indent-tabs-mode: t; c-basic-offset: 4 -*- vim:set ts=4 sw=4 sts=4 noet: */
#ifdef HAVE_CONFIG_H
#include "config.h"
#endif

#include "task.h"

#include <cassert>

namespace flint {
namespace task {

bool Task::IsCanceled() const
{
	return control_mr.get_size() > 0 &&
		control_mr.get_address() &&
		*static_cast<char *>(control_mr.get_address()) == '\1';
}

char *Task::GetControlAddress(int job_id) const
{
	size_t size = control_mr.get_size();
	if (size == 0)
		return nullptr;
	assert(static_cast<size_t>(job_id) < size);
	char *addr = static_cast<char *>(control_mr.get_address());
	return addr + job_id;
}

char *Task::GetProgressAddress(int job_id) const
{
	size_t size = progress_mr.get_size();
	if (size == 0)
		return nullptr;
	assert(static_cast<size_t>(job_id) < size);
	char *addr = static_cast<char *>(progress_mr.get_address());
	return addr + job_id;
}

double *Task::GetRssAddress(int job_id) const
{
	size_t size = rss_mr.get_size();
	if (size == 0)
		return nullptr;
	size_t offset = job_id * sizeof(double);
	assert(offset + sizeof(double) <= size);
	double *addr = static_cast<double *>(rss_mr.get_address());
	return addr + job_id;
}

}
}
