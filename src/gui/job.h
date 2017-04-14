/* -*- Mode: C++; tab-width: 4; indent-tabs-mode: t; c-basic-offset: 4 -*- vim:set ts=4 sw=4 sts=4 noet: */
#ifndef FLINT_GUI_JOB_H_
#define FLINT_GUI_JOB_H_

#pragma GCC diagnostic push
#pragma GCC diagnostic ignored "-Wcast-qual"
#include <wx/filename.h>
#pragma GCC diagnostic pop

namespace flint {
namespace gui {

struct Task;

struct Job {
	const Task &task;
	int id;

	Job(const Task &given_task, int given_id);

	wxFileName GetDirectoryName() const;
	int GetProgress() const;
	bool IsCanceled() const;
	bool IsFinished() const;

	bool RequestCancel() const;
};

}
}

#endif
