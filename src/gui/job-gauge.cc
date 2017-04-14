/* -*- Mode: C++; tab-width: 4; indent-tabs-mode: t; c-basic-offset: 4 -*- vim:set ts=4 sw=4 sts=4 noet: */
#ifdef HAVE_CONFIG_H
#include "config.h"
#endif

#include "gui/job-gauge.h"

#include "gui/job.h"
#include "gui/task.h"

namespace flint {
namespace gui {

JobGauge::JobGauge(wxWindow *parent, Job &job)
	: wxGauge(parent, wxID_ANY, 100)
	, job_(job)
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

void JobGauge::Stop()
{
	if ( GetThread() &&
		 GetThread()->IsRunning() )
		GetThread()->Delete();
}

void JobGauge::OnThreadUpdate(wxThreadEvent &event)
{
	int i = event.GetInt();
	if (0 <= i && i <= 100)
		SetValue(i);
}

wxThread::ExitCode JobGauge::Entry()
{
    while (!GetThread()->TestDestroy()) {
		int p = job_.GetProgress();

		auto *event = new wxThreadEvent;
		event->SetInt(p);
		wxQueueEvent(this, event);

		if (job_.task.IsFinished())
			break;
		wxThread::Sleep(1000);
	}
	return static_cast<wxThread::ExitCode>(0);
}

}
}
