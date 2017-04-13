/* -*- Mode: C++; tab-width: 4; indent-tabs-mode: t; c-basic-offset: 4 -*- vim:set ts=4 sw=4 sts=4 noet: */
#ifdef HAVE_CONFIG_H
#include "config.h"
#endif

#include "gui/job-gauge.h"

#define BOOST_DATE_TIME_NO_LIB
#include <boost/interprocess/file_mapping.hpp>

#include "gui/job.h"

namespace flint {
namespace gui {

JobGauge::JobGauge(wxWindow *parent, const Job &job)
	: wxGauge(parent, wxID_ANY, 100)
	, mr_(job.GetProgressRegion())
{
	Bind(wxEVT_THREAD, &JobGauge::OnThreadUpdate, this);
}

bool JobGauge::Start()
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

void JobGauge::OnThreadUpdate(wxThreadEvent &event)
{
	int i = event.GetInt();
	if (0 < i && i <= 100) {
		Pulse(); // TODO
		SetValue(i);
	}
}

wxThread::ExitCode JobGauge::Entry()
{
    while (!GetThread()->TestDestroy()) {
		int p = static_cast<int>(*reinterpret_cast<char *>(mr_.get_address()));

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
