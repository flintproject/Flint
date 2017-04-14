/* -*- Mode: C++; tab-width: 4; indent-tabs-mode: t; c-basic-offset: 4 -*- vim:set ts=4 sw=4 sts=4 noet: */
#ifdef HAVE_CONFIG_H
#include "config.h"
#endif

#include "gui/task-frame.h"

#include "gui/job-gauge.h"
#include "gui/job.h"
#include "gui/sim-frame.h"
#include "gui/task.h"

namespace flint {
namespace gui {

class JobWindow : public wxWindow {
public:
	JobWindow(wxWindow *parent, const Task &task, int id);

	JobGauge *gauge() {return gauge_;}

private:
	void OnX(wxCommandEvent &event);

	Job job_;
	JobGauge *gauge_;
};

JobWindow::JobWindow(wxWindow *parent, const Task &task, int id)
	: wxWindow(parent, wxID_ANY)
	, job_(task, id)
	, gauge_(new JobGauge(this, job_))
{
	auto x = new wxButton(this, wxID_ANY, "x", wxDefaultPosition, wxDefaultSize, wxBU_EXACTFIT);
	if (job_.IsCanceled())
		x->Disable();

	auto hbox0 = new wxBoxSizer(wxHORIZONTAL);
	hbox0->Add(gauge_, 1 /* horizontally stretchable */);
	hbox0->Add(x);
	auto hbox1 = new wxBoxSizer(wxHORIZONTAL);
	hbox1->Add(new wxButton(this, wxID_ANY, "Export"));
	hbox1->Add(new wxButton(this, wxID_ANY, "View"));
	auto vbox = new wxBoxSizer(wxVERTICAL);
	vbox->Add(hbox0, 0, wxEXPAND);
	vbox->Add(hbox1, 0, wxALIGN_RIGHT);
	SetSizerAndFit(vbox);

	x->Bind(wxEVT_BUTTON, &JobWindow::OnX, this);
}

void JobWindow::OnX(wxCommandEvent &event)
{
	if (job_.RequestCancel()) {
		auto x = wxDynamicCast(event.GetEventObject(), wxButton);
		x->Disable();
	}
}

TaskFrame::TaskFrame(wxWindow *parent, const Task &task)
	: wxFrame(parent, wxID_ANY, wxString::Format("Task %d", task.id))
	, task_(task)
	, job_window_(new JobWindow(this, task, 1))
{
	int n = task.GetNumberOfJobs();
	if (n < 0)
		n = 0;
	auto choice = new wxChoice(this, wxID_ANY);
	for (int i=0;i<n;i++)
		choice->Append(wxString::Format("%d", i+1));
	if (n > 0)
		choice->SetSelection(0);
	auto vbox = new wxBoxSizer(wxVERTICAL);
	vbox->Add(new wxButton(this, wxID_ANY, "Export All"), 0, wxALIGN_RIGHT);
	vbox->Add(job_window_, 0, wxEXPAND /* horizontally stretchable */);
	vbox->Add(choice, 0, wxALIGN_CENTER);
	SetSizerAndFit(vbox);
}

void TaskFrame::Start()
{
	job_window_->gauge()->Start();
}

void TaskFrame::OnClose(wxCommandEvent &)
{
}

}
}
