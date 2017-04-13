/* -*- Mode: C++; tab-width: 4; indent-tabs-mode: t; c-basic-offset: 4 -*- vim:set ts=4 sw=4 sts=4 noet: */
#ifndef FLINT_GUI_TASK_H_
#define FLINT_GUI_TASK_H_

#pragma GCC diagnostic push
#pragma GCC diagnostic ignored "-Wcast-qual"
#include <wx/filename.h>
#pragma GCC diagnostic pop

namespace flint {
namespace gui {

struct Simulation;

struct Task {
	Simulation *simulation;
	int id;

	Task(Simulation *given_simulation, int given_id);

	wxFileName GetDirectoryName() const;
	int GetNumberOfJobs() const;
};

}
}

#endif
