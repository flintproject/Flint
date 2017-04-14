/* -*- Mode: C++; tab-width: 4; indent-tabs-mode: t; c-basic-offset: 4 -*- vim:set ts=4 sw=4 sts=4 noet: */
#ifdef HAVE_CONFIG_H
#include "config.h"
#endif

#include "gui/task.h"

#include "gui/job.h"
#include "gui/simulation.h"

namespace flint {
namespace gui {

Task::Task(Simulation *given_simulation, int given_id)
	: simulation(given_simulation)
	, id(given_id)
{}

wxFileName Task::GetDirectoryName() const
{
	auto filename = simulation->GetDirectoryName();
	filename.AppendDir(wxString::Format("%d", id));
	return filename;
}

int Task::GetNumberOfJobs() const
{
	return static_cast<int>(simulation->GetProgressFileName(id).GetSize().ToULong()-1);
}

bool Task::RequestCancel() const
{
	auto filename = GetDirectoryName();
	filename.SetFullName("canceled");
	wxFile file;
	if (!file.Create(filename.GetFullPath(), true))
		return false;
	for (int i=GetNumberOfJobs();i>0;i--) {
		Job job(*this, i); // base 1
		job.RequestCancel();
	}
	return true;
}

}
}
