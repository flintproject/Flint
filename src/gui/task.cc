/* -*- Mode: C++; tab-width: 4; indent-tabs-mode: t; c-basic-offset: 4 -*- vim:set ts=4 sw=4 sts=4 noet: */
#ifdef HAVE_CONFIG_H
#include "config.h"
#endif

#include "gui/task.h"

#include "gui/configuration.h"
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

wxString Task::GetDpsPath() const
{
	return simulation->entries[id-1].second->dps_path;
}

bool Task::HasObjective() const
{
	return !GetDpsPath().empty();
}

int Task::GetNumberOfJobs() const
{
	return static_cast<int>(simulation->GetProgressFileName(id).GetSize().ToULong()-1);
}

namespace {

wxFileName GetCanceledFileName(const Task *task)
{
	auto filename = task->GetDirectoryName();
	filename.SetFullName("canceled");
	return filename;
}

}

bool Task::IsCanceled() const
{
	auto filename = GetCanceledFileName(this);
	return filename.FileExists();
}

bool Task::IsFinished() const
{
	if (IsCanceled())
		return true;
	auto filename = simulation->GetProgressFileName(id);
	wxFile file(filename.GetFullPath());
	if (!file.IsOpened())
		return false;
	char c;
	if (file.Read(&c, 1) == 0)
		return false;
	file.Close();
	return c == 100; // TODO: is it better to check if all of jobs are finished too?
}

bool Task::RequestCancel() const
{
	wxFile file;
	if (!file.Create(GetCanceledFileName(this).GetFullPath(), true))
		return false;
	for (int i=GetNumberOfJobs();i>0;i--) {
		Job job(*this, i); // base 1
		job.RequestCancel();
	}
	return true;
}

}
}
