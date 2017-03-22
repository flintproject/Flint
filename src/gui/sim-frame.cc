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
#include "gui/main-frame.h"
#include "gui/phsp.h"
#include "gui/sedml.h"
#include "gui/simulation.h"
#include "gui/task-gauge.h"

namespace flint {
namespace gui {

namespace {

class TaskWindow : public wxWindow {
public:
	TaskWindow(wxWindow *parent, Simulation *sim, int i);

	TaskGauge *gauge() {return gauge_;}

private:
	TaskGauge *gauge_;
};

TaskWindow::TaskWindow(wxWindow *parent, Simulation *sim, int i)
	: wxWindow(parent, wxID_ANY)
{
	gauge_ = new TaskGauge(this, sim->GetProgressFileName(i));
	auto x = new wxButton(this, wxID_ANY, "x", wxDefaultPosition, wxDefaultSize, wxBU_EXACTFIT);
	auto hbox = new wxStaticBoxSizer(wxHORIZONTAL, this, wxString::Format("Task %d", i));
	hbox->Add(new wxButton(this, wxID_ANY, "Detail"));
	hbox->Add(gauge_, 1 /* horizontally stretchable */);
	hbox->Add(x);
	SetSizerAndFit(hbox);
}

}

SimFrame::SimFrame(MainFrame *parent, Simulation *sim)
	: wxFrame(parent, wxID_ANY, wxString::Format("Simulation %d", sim->id))
	, sim_(sim)
{
	auto vbox = new wxBoxSizer(wxVERTICAL);
	int i = 0;
	for (auto &p : sim_->entries) {
		auto window = new TaskWindow(this, sim, ++i);
		vbox->Add(window, 0, wxEXPAND /* horizontally stretchable */);
		gauges_.push_back(window->gauge());
	}
	SetSizerAndFit(vbox);
	SetClientSize(GetSize());

	Bind(wxEVT_THREAD, &SimFrame::OnThreadUpdate, this);
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

	for (auto *gauge : gauges_)
		if (!gauge->Start())
			return false;
	return true;
}

void SimFrame::OnThreadUpdate(wxThreadEvent &)
{
	if (result_)
		wxLogInfo("simulation finished successfully");
	else
		wxLogError("simulation failed: %s", ec_.Get());
}

wxThread::ExitCode SimFrame::Entry()
{
	wxFileName orig_dir, temp_dir;

	// change a dedicated directory for the simulation
	orig_dir.AssignCwd();
	temp_dir = sim_->GetDirectoryName();
	temp_dir.Mkdir(wxS_DIR_DEFAULT, wxPATH_MKDIR_FULL); // make sure that it exists
	temp_dir.SetCwd();

	result_ = exec::Exec(option_);

	// recover current directory
	orig_dir.SetCwd();

	wxQueueEvent(this, new wxThreadEvent);
	return static_cast<wxThread::ExitCode>(0);
}

void SimFrame::OnClose(wxCommandEvent &)
{
}

}
}
