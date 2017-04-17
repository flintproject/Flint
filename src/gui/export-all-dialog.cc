/* -*- Mode: C++; tab-width: 4; indent-tabs-mode: t; c-basic-offset: 4 -*- vim:set ts=4 sw=4 sts=4 noet: */
#ifdef HAVE_CONFIG_H
#include "config.h"
#endif

#include "gui/export-all-dialog.h"

#include <cstdlib>
#include <iostream>
#include <fstream>

#include "flint/error.h"
#include "gui/job.h"
#include "gui/task.h"
#include "gui/task-frame.h"
#include "isd2csv.h"

namespace flint {
namespace gui {

ExportAllDialog::ExportAllDialog(TaskFrame *frame, const wxString &target_path, int type)
	: wxProgressDialog("Exporting files", // title
					   "Exporting files", // message
					   100, // maximum
					   frame)
	, frame_(frame)
	, target_path_(target_path)
	, type_(type)
{
	Bind(wxEVT_THREAD, &ExportAllDialog::OnThreadUpdate, this);
}

bool ExportAllDialog::Start()
{
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

void ExportAllDialog::OnThreadUpdate(wxThreadEvent &event)
{
	int i = event.GetInt();
	if (i < 0) {
		ShowErrorOnExporting(event.GetString());
	} else if (i == 0) {
		Pulse();
	} else if (i <= 100) {
		Update(i);
	}
}

wxThread::ExitCode ExportAllDialog::Entry()
{
	wxThreadEvent *event;
	int n = frame_->task().GetNumberOfJobs();
	for (int i=0;i<n;i++) {
		if (GetThread()->TestDestroy())
			break;

		event = new wxThreadEvent;
		event->SetInt((i * 100)/n);
		wxQueueEvent(this, event);

		Job job(frame_->task(), i+1);
		if (!job.IsFinished())
			continue;
		auto source_file = job.GetOutputFileName();
		if (!source_file.FileExists())
			continue;
		wxFileName filename(target_path_,
							(type_ == 0) ? wxString::Format("%d.csv", i+1) : wxString::Format("%d.isd", i+1));
		auto target_path = filename.GetFullPath();

		if (type_ == 0) { // CSV
			std::ifstream ifs(source_file.GetFullPath().c_str(), std::ios::in|std::ios::binary);
			if (!ifs.is_open()) {
				event = new wxThreadEvent;
				event->SetInt(-1);
				event->SetString(wxString::Format("failed to open %s", source_file.GetFullPath()));
				wxQueueEvent(this, event);
				continue;
			}
			std::ofstream ofs(target_path.c_str(), std::ios::out);
			if (!ofs.is_open()) {
				ifs.close();
				event = new wxThreadEvent;
				event->SetInt(-1);
				event->SetString(wxString::Format("failed to open %s", target_path));
				wxQueueEvent(this, event);
				continue;
			}
			isd2csv::Option option;
			option.ignore_prefixes = false;
			option.ignore_units = false;
			StderrCapture ec;
			int b = isd2csv::Convert(option, &ifs, &ofs) == EXIT_SUCCESS;
			ofs.close();
			ifs.close();
			if (!b) {
				event = new wxThreadEvent;
				event->SetInt(-1);
				event->SetString(wxString::Format("failed to export %s: %s", target_path, ec.Get()));
				wxQueueEvent(this, event);
				continue;
			}
		} else { // ISD
			if (!wxCopyFile(source_file.GetFullPath(), target_path)) {
				event = new wxThreadEvent;
				event->SetInt(-1);
				event->SetString(wxString::Format("failed to export %s", target_path));
				wxQueueEvent(this, event);
				continue;
			}
		}
	}
	event = new wxThreadEvent;
	event->SetInt(100);
	wxQueueEvent(this, event);
	return static_cast<wxThread::ExitCode>(0);
}

void ExportAllDialog::ShowErrorOnExporting(const wxString &message)
{
	wxMessageBox(message, "Error on exporting", wxOK|wxICON_ERROR, this);
}

}
}
