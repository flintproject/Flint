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

	void SwitchJobId(int id);

private:
	void OnX(wxCommandEvent &event);

	Job job_;
	JobGauge *gauge_;
	wxButton *x_;
};

JobWindow::JobWindow(wxWindow *parent, const Task &task, int id)
	: wxWindow(parent, wxID_ANY)
	, job_(task, id)
	, gauge_(new JobGauge(this, job_))
	, x_(new wxButton(this, wxID_ANY, "x", wxDefaultPosition, wxDefaultSize, wxBU_EXACTFIT))
{
	if (job_.IsFinished())
		x_->Disable();

	auto hbox0 = new wxBoxSizer(wxHORIZONTAL);
	hbox0->Add(gauge_, 1 /* horizontally stretchable */);
	hbox0->Add(x_);
	auto hbox1 = new wxBoxSizer(wxHORIZONTAL);
	hbox1->Add(new wxButton(this, wxID_ANY, "Export"));
	hbox1->Add(new wxButton(this, wxID_ANY, "View"));
	auto vbox = new wxBoxSizer(wxVERTICAL);
	vbox->Add(hbox0, 0, wxEXPAND);
	vbox->Add(hbox1, 0, wxALIGN_RIGHT);
	SetSizerAndFit(vbox);

	x_->Bind(wxEVT_BUTTON, &JobWindow::OnX, this);
}

void JobWindow::SwitchJobId(int id)
{
	job_.id = id;
	x_->Enable(!job_.IsFinished());
}

void JobWindow::OnX(wxCommandEvent &event)
{
	if (job_.IsFinished() || job_.RequestCancel()) {
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

	choice->Bind(wxEVT_CHOICE, &TaskFrame::OnChoice, this);
	Bind(wxEVT_CLOSE_WINDOW, &TaskFrame::OnClose, this);
}

void TaskFrame::Start()
{
	job_window_->gauge()->Start();
}

void TaskFrame::OnChoice(wxCommandEvent &event)
{
	auto choice = wxDynamicCast(event.GetEventObject(), wxChoice);
	int i = choice->GetSelection();
	if (i == wxNOT_FOUND)
		return;
	job_window_->SwitchJobId(i+1);
}

void TaskFrame::OnClose(wxCloseEvent &)
{
	job_window_->gauge()->Stop();
	Destroy();
}

}
}
