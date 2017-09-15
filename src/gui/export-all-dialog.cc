/* -*- Mode: C++; tab-width: 4; indent-tabs-mode: t; c-basic-offset: 4 -*- vim:set ts=4 sw=4 sts=4 noet: */
#ifdef HAVE_CONFIG_H
#include "config.h"
#endif

#include "gui/export-all-dialog.h"

#include <cstdlib>
#include <iostream>

#define BOOST_FILESYSTEM_NO_DEPRECATED
#include <boost/filesystem.hpp>

#include "flint/error.h"
#include "gui/filename.h"
#include "gui/job.h"
#include "gui/task.h"
#include "gui/task-frame.h"
#include "isd2csv.h"

namespace flint {
namespace gui {

ExportAllDialog::ExportAllDialog(TaskFrame *frame, const wxString &target_path, int type,
								 std::vector<int> &&indice)
	: wxProgressDialog("Exporting files", // title
					   "Exporting files", // message
					   100, // maximum
					   frame)
	, frame_(frame)
	, target_path_(target_path)
	, type_(type)
	, indice_(std::move(indice))
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
		Destroy();
	} else if (i == 0) {
		Pulse();
	} else if (i < 100) {
		Update(i);
	} else {
		Destroy();
	}
}

wxThread::ExitCode ExportAllDialog::Entry()
{
	wxThreadEvent *event;
	int k = 0;
	for (int i : indice_) {
		if (GetThread()->TestDestroy())
			break;

		event = new wxThreadEvent;
		event->SetInt((k++ * 100)/indice_.size());
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
			boost::filesystem::ifstream ifs(GetPathFromWxFileName(source_file), std::ios::in|std::ios::binary);
			if (!ifs.is_open()) {
				event = new wxThreadEvent;
				event->SetInt(-1);
				event->SetString(wxString::Format("failed to open %s", source_file.GetFullPath()));
				wxQueueEvent(this, event);
				return static_cast<wxThread::ExitCode>(0);
			}
			boost::filesystem::ofstream ofs(GetPathFromWxFileName(filename), std::ios::out|std::ios::binary);
			if (!ofs.is_open()) {
				ifs.close();
				event = new wxThreadEvent;
				event->SetInt(-1);
				event->SetString(wxString::Format("failed to open %s", target_path));
				wxQueueEvent(this, event);
				return static_cast<wxThread::ExitCode>(0);
			}
			isd2csv::Option option;
			option.ignore_prefixes = false;
			option.ignore_units = false;
			option.port = ""; // no progress report
			StderrCapture ec;
			int b = isd2csv::Convert(option, &ifs, &ofs) == EXIT_SUCCESS;
			ofs.close();
			ifs.close();
			if (!b) {
				event = new wxThreadEvent;
				event->SetInt(-1);
				event->SetString(wxString::Format("failed to export %s: %s", target_path, ec.Get()));
				wxQueueEvent(this, event);
				return static_cast<wxThread::ExitCode>(0);
			}
		} else { // ISD
			if (!wxCopyFile(source_file.GetFullPath(), target_path)) {
				event = new wxThreadEvent;
				event->SetInt(-1);
				event->SetString(wxString::Format("failed to export %s", target_path));
				wxQueueEvent(this, event);
				return static_cast<wxThread::ExitCode>(0);
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
