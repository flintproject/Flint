/* -*- Mode: C++; tab-width: 4; indent-tabs-mode: t; c-basic-offset: 4 -*- vim:set ts=4 sw=4 sts=4 noet: */
#ifdef HAVE_CONFIG_H
#include "config.h"
#endif

#include "gui/task-gauge.h"

#define BOOST_DATE_TIME_NO_LIB
#include <boost/interprocess/file_mapping.hpp>
#include <boost/interprocess/mapped_region.hpp>

#include "gui/filename.h"

namespace flint {
namespace gui {

TaskGauge::TaskGauge(wxWindow *parent, wxFileName filename)
	: wxGauge(parent, wxID_ANY, 100, wxDefaultPosition, wxSize(120, 10))
	, filename_(filename)
{
	Bind(wxEVT_THREAD, &TaskGauge::OnThreadUpdate, this);
}

bool TaskGauge::Start()
{
	// start thread
	if (CreateThread(wxTHREAD_DETACHED) != wxTHREAD_NO_ERROR) {
		wxLogError("failed to create the helper thread");
		return false;
	}
	if (GetThread()->Run() != wxTHREAD_NO_ERROR) {
		wxLogError("failed to run the helper thread");
		return false;
	}
	return true;
}

void TaskGauge::Stop()
{
	if ( GetThread() &&
		 GetThread()->IsRunning() )
		GetThread()->Delete();
}

void TaskGauge::OnThreadUpdate(wxThreadEvent &event)
{
	int i = event.GetInt();
	if (i <= 0) {
		Pulse();
	} else if (i <= 100) {
		SetValue(i);
	}
}

wxThread::ExitCode TaskGauge::Entry()
{
	boost::interprocess::mapped_region mr;

	// wait until progress files are ready
	for (;;) {
		try {
			boost::interprocess::file_mapping fm(GetFnStrFromWxFileName(filename_).c_str(),
												 boost::interprocess::read_only);
			mr = boost::interprocess::mapped_region(fm, boost::interprocess::read_only, 0, 1);
			break;
		} catch (const boost::interprocess::interprocess_exception &e) {
			// wait for retry
			wxThread::Sleep(1000);
		}
	}

    while (!GetThread()->TestDestroy()) {
		int p = static_cast<int>(*static_cast<char *>(mr.get_address()));

		auto *event = new wxThreadEvent;
		event->SetInt(p);
		wxQueueEvent(this, event);

		if (p == 100)
			break;
		wxThread::Sleep(1000);
	}
	return static_cast<wxThread::ExitCode>(0);
}

}
}
