/* -*- Mode: C++; tab-width: 4; indent-tabs-mode: t; c-basic-offset: 4 -*- vim:set ts=4 sw=4 sts=4 noet: */
#ifdef HAVE_CONFIG_H
#include "config.h"
#endif

#include "gui/task-window.h"

#include "gui/filename.h"
#include "gui/simulation.h"
#include "gui/task-frame.h"
#include "gui/task-gauge.h"
#include "gui/task.h"

namespace flint {
namespace gui {

void TaskWindow::NotifyClose()
{
	gauge_->Stop();
	if (!task_.IsFinished())
		task_.RequestCancel();
}

TaskWindow::TaskWindow(wxWindow *parent, Simulation *sim, int i)
	: wxWindow(parent, wxID_ANY)
	, task_(sim, i)
{
	auto hbox = new wxStaticBoxSizer(wxHORIZONTAL, this, wxString::Format("Task %d", i));
	gauge_ = new TaskGauge(hbox->GetStaticBox(), task_.GetProgressFileName());
	auto x = new wxButton(hbox->GetStaticBox(), wxID_ANY, "x", wxDefaultPosition, wxDefaultSize, wxBU_EXACTFIT);
	auto detail = new wxButton(hbox->GetStaticBox(), wxID_ANY, "Detail");
	hbox->Add(detail);
	hbox->Add(gauge_, 1 /* horizontally stretchable */);
	hbox->Add(x);
	SetSizerAndFit(hbox);

	detail->Bind(wxEVT_BUTTON, &TaskWindow::OnDetail, this);
	x->Bind(wxEVT_BUTTON, &TaskWindow::OnX, this);
}

void TaskWindow::OnDetail(wxCommandEvent &)
{
	auto *frame = new TaskFrame(GetParent(), task_);
	if (!frame->LoadItems()) {
		delete frame;
		return;
	}
	frame->CentreOnParent();
	frame->Show();
	frame->Start();
}

void TaskWindow::OnX(wxCommandEvent &event)
{
	if (task_.IsFinished() || task_.RequestCancel()) {
		auto x = wxDynamicCast(event.GetEventObject(), wxButton);
		x->Disable();
	}
}

}
}
