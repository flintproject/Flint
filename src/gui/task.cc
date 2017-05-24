/* -*- Mode: C++; tab-width: 4; indent-tabs-mode: t; c-basic-offset: 4 -*- vim:set ts=4 sw=4 sts=4 noet: */
#ifdef HAVE_CONFIG_H
#include "config.h"
#endif

#include "gui/task.h"

#define BOOST_DATE_TIME_NO_LIB
#include <boost/interprocess/file_mapping.hpp>
#include <boost/interprocess/mapped_region.hpp>

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

wxFileName Task::GetControlFileName() const
{
	auto filename = GetDirectoryName();
	filename.SetFullName("control");
	return filename;
}

wxFileName Task::GetProgressFileName() const
{
	auto filename = GetDirectoryName();
	filename.SetFullName("progress");
	return filename;
}

wxFileName Task::GetRssFileName() const
{
	auto filename = GetDirectoryName();
	filename.SetFullName("rss");
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
	return static_cast<int>(GetProgressFileName().GetSize().ToULong()-1);
}

bool Task::IsCanceled() const
{
	wxFile file(GetControlFileName().GetFullPath(), wxFile::read);
	char c = '\0';
	return file.IsOpened() && file.Read(&c, 1) == 1u && c == '\1' && file.Close();
}

bool Task::IsFinished() const
{
	if (IsCanceled())
		return true;
	auto filename = GetProgressFileName();
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
	try {
		auto filename = GetControlFileName();
		boost::interprocess::file_mapping fm(filename.GetFullPath().c_str(), // TODO: check locale-dependency
											 boost::interprocess::read_write);
		boost::interprocess::mapped_region mr(fm, boost::interprocess::read_write);
		char *p = static_cast<char *>(mr.get_address());
		for (size_t i=0;i<mr.get_size();i++)
			p[i] = '\1';
		return true;
	} catch (const boost::interprocess::interprocess_exception &) {
		return false;
	}
}

}
}
