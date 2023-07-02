/* -*- Mode: C++; tab-width: 4; indent-tabs-mode: t; c-basic-offset: 4 -*- vim:set ts=4 sw=4 sts=4 noet: */
#ifdef HAVE_CONFIG_H
#include "config.h"
#endif

#include "gui/sim-window.h"

#pragma GCC diagnostic push
#pragma GCC diagnostic ignored "-Wcast-qual"
#include <wx/filename.h>
#include <wx/log.h>
#pragma GCC diagnostic pop

#include "exec.h"
#include "flint/error.h"
#include "gui/filename.h"
#include "gui/log-dialog.h"
#include "gui/main-frame.h"
#include "gui/phsp.h"
#include "gui/sedml.h"
#include "gui/simulation.h"
#include "gui/task-frame.h"
#include "gui/task-gauge.h"
#include "gui/task-window.h"
#include "gui/task.h"

namespace flint {
namespace gui {

SimWindow::SimWindow(MainFrame *parent, Simulation *sim)
	: wxWindow(parent, wxID_ANY, wxDefaultPosition, wxDefaultSize, 0, wxString::Format("Simulation %d", sim->id))
	, sim_(sim)
	, log_dialog_(new LogDialog(this, wxString::Format("Job %d's log", sim->id)))
	, log_button_(new wxButton(this, wxID_ANY, "Log"))
{
	auto vbox = new wxBoxSizer(wxVERTICAL);
	int i = 0;
	for (auto &p : sim_->entries) {
		(void)p;
		auto window = new TaskWindow(this, sim, ++i);
		vbox->Add(window, 0, wxEXPAND /* horizontally stretchable */);
		windows_.push_back(window);
	}
	vbox->Add(log_button_, wxSizerFlags(0).Center());
	SetSizerAndFit(vbox);
	log_button_->Hide();

	log_button_->Bind(wxEVT_BUTTON, &SimWindow::OnLog, this);
	Bind(wxEVT_THREAD, &SimWindow::OnThreadUpdate, this);
}

SimWindow::~SimWindow()
{
	for (auto *window : windows_)
		window->NotifyClose();
	if ( GetThread() &&
		 GetThread()->IsRunning() )
		GetThread()->Delete();
}

bool SimWindow::Start(int concurrency)
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
	option_.set_concurrency(concurrency);

	// start thread
	if (CreateThread(wxTHREAD_DETACHED) != wxTHREAD_NO_ERROR) {
		wxLogError("failed to create the helper thread");
		return false;
	}
	if (GetThread()->Run() != wxTHREAD_NO_ERROR) {
		wxLogError("failed to run the helper thread");
		return false;
	}

	Show();

	for (auto *window : windows_)
		if (!window->gauge()->Start())
			return false;
	return true;
}

void SimWindow::OnThreadUpdate(wxThreadEvent &)
{
	auto *main_frame = wxDynamicCast(GetParent(), MainFrame);

	auto log_file = sim_->GetLogFileName();
	if (log_file.FileExists() && log_file.GetSize() > 0) {
		log_dialog_->text_ctrl()->LoadFile(GetFnStrFromWxFileName(log_file));
		log_button_->Show();
	}
	if (result_)
		main_frame->SetStatusText(wxString::Format("Job %d finished successfully", sim_->id));
	else {
		main_frame->SetStatusText(wxString::Format("Job %d failed", sim_->id));
		log_dialog_->Show();
	}

	main_frame->ResetControl();
}

wxThread::ExitCode SimWindow::Entry()
{
	auto sim_dir = sim_->GetDirectoryName();
	sim_dir.Mkdir(wxS_DIR_DEFAULT, wxPATH_MKDIR_FULL); // make sure that it exists

	auto *main_frame = wxDynamicCast(GetParent(), MainFrame);
	{
		auto log_path = GetPathFromWxFileName(sim_->GetLogFileName());
		boost::filesystem::ofstream ofs(log_path, std::ios::binary);
		StderrRedirect redirect(&ofs);
		result_ = exec::Exec(option_, GetPathFromWxFileName(sim_->GetDirectoryName()), &main_frame->arg());
	}

	wxQueueEvent(this, new wxThreadEvent);
	return static_cast<wxThread::ExitCode>(0);
}

void SimWindow::OnLog(wxCommandEvent &)
{
	log_dialog_->Show();
}

}
}
