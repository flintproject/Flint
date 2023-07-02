/* -*- Mode: C++; tab-width: 4; indent-tabs-mode: t; c-basic-offset: 4 -*- vim:set ts=4 sw=4 sts=4 noet: */
#ifdef HAVE_CONFIG_H
#include "config.h"
#endif

#include "gui/job.h"

#define BOOST_DATE_TIME_NO_LIB
#include <boost/interprocess/file_mapping.hpp>
#include <boost/interprocess/mapped_region.hpp>

#include "gui/filename.h"
#include "gui/simulation.h"
#include "gui/task.h"

namespace flint {
namespace gui {

Job::Job(const Task &given_task, int given_id)
	: task(given_task)
	, id(given_id)
{}

wxFileName Job::GetDirectoryName() const
{
	auto filename = task.GetDirectoryName();
	unsigned int u = static_cast<unsigned int>(id);
	unsigned int a, b, c, d;
	a = (u>>24)&0xFF;
	b = (u>>16)&0xFF;
	c = (u>> 8)&0xFF;
	d = (u    )&0xFF;
	filename.AppendDir(wxString::Format("%02x", a));
	filename.AppendDir(wxString::Format("%02x", b));
	filename.AppendDir(wxString::Format("%02x", c));
	filename.AppendDir(wxString::Format("%02x", d));
	return filename;
}

wxFileName Job::GetOutputFileName() const
{
	auto filename = GetDirectoryName();
	filename.SetFullName("out.isd");
	return filename;
}

int Job::GetProgress() const
{
	try {
		auto filename = task.GetProgressFileName();
		boost::interprocess::file_mapping fm(GetFnStrFromWxFileName(filename).c_str(),
											 boost::interprocess::read_only);
		boost::interprocess::mapped_region mr(fm, boost::interprocess::read_only, id, 1);
		return static_cast<int>(*static_cast<char *>(mr.get_address()));
	} catch (const boost::interprocess::interprocess_exception &) {
		return 0;
	}
}

bool Job::IsCanceled() const
{
	try {
		auto filename = task.GetControlFileName();
		boost::interprocess::file_mapping fm(GetFnStrFromWxFileName(filename).c_str(),
											 boost::interprocess::read_only);
		boost::interprocess::mapped_region mr(fm, boost::interprocess::read_only, id, 1);
		return *static_cast<char *>(mr.get_address()) == 1;
	} catch (const boost::interprocess::interprocess_exception &) {
		return false;
	}
}

bool Job::IsFinished() const
{
	return IsCanceled() || GetProgress() == 100;
}

bool Job::RequestCancel() const
{
	wxFile file(task.GetControlFileName().GetFullPath(), wxFile::read_write);
	return file.IsOpened() &&
		file.Seek(id) == id &&
		file.Write("\1", 1) == 1u &&
		file.Close();
}

}
}
