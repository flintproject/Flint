/* -*- Mode: C++; tab-width: 4; indent-tabs-mode: t; c-basic-offset: 4 -*- vim:set ts=4 sw=4 sts=4 noet: */
#ifdef HAVE_CONFIG_H
#include "config.h"
#endif

#include "gui/sim-frame.h"

#pragma GCC diagnostic push
#pragma GCC diagnostic ignored "-Wcast-qual"
#include <wx/filename.h>
#pragma GCC diagnostic pop

#include "exec.h"
#include "gui/filename.h"
#include "gui/main-frame.h"
#include "gui/phsp.h"
#include "gui/sedml.h"
#include "gui/simulation.h"
#include "gui/task-frame.h"
#include "gui/task-gauge.h"
#include "gui/task.h"

namespace flint {
namespace gui {

class TaskWindow : public wxWindow {
public:
	TaskWindow(wxWindow *parent, Simulation *sim, int i);

	TaskGauge *gauge() {return gauge_;}

	void NotifyClose();

private:
	void OnDetail(wxCommandEvent &event);
	void OnX(wxCommandEvent &event);

	TaskGauge *gauge_;
	Task task_;
};

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

SimFrame::SimFrame(MainFrame *parent, Simulation *sim)
	: wxFrame(parent, wxID_ANY, wxString::Format("Simulation %d", sim->id))
	, sim_(sim)
{
	auto vbox = new wxBoxSizer(wxVERTICAL);
	int i = 0;
	for (auto &p : sim_->entries) {
		(void)p;
		auto window = new TaskWindow(this, sim, ++i);
		vbox->Add(window, 0, wxEXPAND /* horizontally stretchable */);
		windows_.push_back(window);
	}
	CreateStatusBar(1, wxSTB_DEFAULT_STYLE, wxID_ANY);
	SetSizerAndFit(vbox);

	Bind(wxEVT_THREAD, &SimFrame::OnThreadUpdate, this);
	Bind(wxEVT_CLOSE_WINDOW, &SimFrame::OnClose, this);
}

bool SimFrame::Start()
{
	wxFileName filename = sim_->GetDirectoryName();
	filename.Mkdir(wxS_DIR_DEFAULT, wxPATH_MKDIR_FULL); // make sure that it exists

	// prepare input SED-ML/PHSP files
	filename.SetFullName("input.xml");
	if (!WriteSedml(sim_, filename))
		return false;
	option_.set_sedml_filename(filename.GetFullPath().utf8_str().data());
	filename.SetFullName("input.phsp");
	if (!WritePhsp(sim_, filename))
		return false;
	option_.set_phsp_filename(filename.GetFullPath().utf8_str().data());

	// start thread
	if (CreateThread(wxTHREAD_DETACHED) != wxTHREAD_NO_ERROR) {
		wxLogError("failed to create the helper thread");
		return false;
	}
	if (GetThread()->Run() != wxTHREAD_NO_ERROR) {
		wxLogError("failed to run the helper thread");
		return false;
	}

	CentreOnParent();
	Show();

	for (auto *window : windows_)
		if (!window->gauge()->Start())
			return false;
	return true;
}

void SimFrame::OnThreadUpdate(wxThreadEvent &)
{
	if (result_)
		SetStatusText("finished successfully.");
	else
		wxLogError("simulation failed: %s", ec_.Get());

	auto *main_frame = wxDynamicCast(GetParent(), MainFrame);
	main_frame->ResetControl();
}

wxThread::ExitCode SimFrame::Entry()
{
	auto sim_dir = sim_->GetDirectoryName();
	sim_dir.Mkdir(wxS_DIR_DEFAULT, wxPATH_MKDIR_FULL); // make sure that it exists

	auto *main_frame = wxDynamicCast(GetParent(), MainFrame);
	result_ = exec::Exec(option_, GetPathFromWxFileName(sim_dir), &main_frame->arg());

	wxQueueEvent(this, new wxThreadEvent);
	return static_cast<wxThread::ExitCode>(0);
}

void SimFrame::OnClose(wxCloseEvent &)
{
	for (auto *window : windows_)
		window->NotifyClose();
	if ( GetThread() &&
		 GetThread()->IsRunning() )
		GetThread()->Delete();
	Destroy();
}

}
}
