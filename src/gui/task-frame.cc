/* -*- Mode: C++; tab-width: 4; indent-tabs-mode: t; c-basic-offset: 4 -*- vim:set ts=4 sw=4 sts=4 noet: */
#ifdef HAVE_CONFIG_H
#include "config.h"
#endif

#include "gui/task-frame.h"

#include <cstdlib>
#include <iostream>
#include <fstream>

#include "flint/error.h"
#include "gui/export-all-dialog.h"
#include "gui/job-gauge.h"
#include "gui/job.h"
#include "gui/sim-frame.h"
#include "gui/task.h"
#include "isd2csv.h"

namespace flint {
namespace gui {

class JobWindow : public wxWindow {
public:
	JobWindow(wxWindow *parent, const Task &task, int id);

	JobGauge *gauge() {return gauge_;}

	void SwitchJobId(int id);

private:
	void OnX(wxCommandEvent &event);
	void OnExport(wxCommandEvent &event);

	void ShowErrorOnExporting(const wxString &message)
	{
		wxMessageBox(message, "Error on exporting", wxOK|wxICON_ERROR, this);
	}

	Job job_;
	JobGauge *gauge_;
	wxButton *x_;
	wxButton *export_;
};

JobWindow::JobWindow(wxWindow *parent, const Task &task, int id)
	: wxWindow(parent, wxID_ANY)
	, job_(task, id)
	, gauge_(new JobGauge(this, job_))
	, x_(new wxButton(this, wxID_ANY, "x", wxDefaultPosition, wxDefaultSize, wxBU_EXACTFIT))
	, export_(new wxButton(this, wxID_ANY, "Export"))
{
	bool finished = job_.IsFinished();
	x_->Enable(!finished);
	export_->Enable(finished);

	auto hbox0 = new wxBoxSizer(wxHORIZONTAL);
	hbox0->Add(gauge_, 1 /* horizontally stretchable */);
	hbox0->Add(x_);
	auto hbox1 = new wxBoxSizer(wxHORIZONTAL);
	hbox1->Add(export_);
	hbox1->Add(new wxButton(this, wxID_ANY, "View"));
	auto vbox = new wxBoxSizer(wxVERTICAL);
	vbox->Add(hbox0, 0, wxEXPAND);
	vbox->Add(hbox1, 0, wxALIGN_RIGHT);
	SetSizerAndFit(vbox);

	x_->Bind(wxEVT_BUTTON, &JobWindow::OnX, this);
	export_->Bind(wxEVT_BUTTON, &JobWindow::OnExport, this);
}

void JobWindow::SwitchJobId(int id)
{
	job_.id = id;
	bool finished = job_.IsFinished();
	x_->Enable(!finished);
	export_->Enable(finished);
}

void JobWindow::OnX(wxCommandEvent &event)
{
	if (job_.IsFinished() || job_.RequestCancel()) {
		auto x = wxDynamicCast(event.GetEventObject(), wxButton);
		x->Disable();
	}
}

void JobWindow::OnExport(wxCommandEvent &)
{
	if (!job_.IsFinished())
		return;
	auto source_file = job_.GetOutputFileName();
	if (!source_file.FileExists()) {
		ShowErrorOnExporting(wxString::Format("missing output file of job %d", job_.id));
		return;
	}
	wxArrayString arr;
	arr.Add("CSV");
	arr.Add("ISD");
	int r = wxGetSingleChoiceIndex("Choose a file format", "Export to", arr, this);
	if (r == -1) // cancelled
		return;
	wxFileDialog saveFileDialog(this,
								"Target file",
								"",
								"",
								"*.*",
								wxFD_SAVE|wxFD_OVERWRITE_PROMPT);
	if (saveFileDialog.ShowModal() == wxID_CANCEL)
		return;
	auto target_path = saveFileDialog.GetPath();
	if (r == 0) { // CSV
		std::ifstream ifs(source_file.GetFullPath().c_str(), std::ios::in|std::ios::binary);
		if (!ifs.is_open()) {
			ShowErrorOnExporting(wxString::Format("failed to open %s", source_file.GetFullPath()));
			return;
		}
		std::ofstream ofs(target_path.c_str(), std::ios::out);
		if (!ofs.is_open()) {
			ifs.close();
			ShowErrorOnExporting(wxString::Format("failed to open %s", target_path));
			return;
		}
		isd2csv::Option option;
		option.ignore_prefixes = false;
		option.ignore_units = false;
		StderrCapture ec;
		int b = isd2csv::Convert(option, &ifs, &ofs) == EXIT_SUCCESS;
		ofs.close();
		ifs.close();
		if (!b) {
			ShowErrorOnExporting(wxString::Format("failed to export %s: %s", target_path, ec.Get()));
			return;
		}
	} else { // ISD
		if (!wxCopyFile(source_file.GetFullPath(), target_path)) {
			ShowErrorOnExporting(wxString::Format("failed to export %s", target_path));
			return;
		}
	}
	wxMessageBox(wxString::Format("exported to %s successuflly", target_path), "Exported");
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
	auto export_all = new wxButton(this, wxID_ANY, "Export All");
	vbox->Add(export_all, 0, wxALIGN_RIGHT);
	vbox->Add(job_window_, 0, wxEXPAND /* horizontally stretchable */);
	vbox->Add(choice, 0, wxALIGN_CENTER);
	SetSizerAndFit(vbox);

	choice->Bind(wxEVT_CHOICE, &TaskFrame::OnChoice, this);
	export_all->Bind(wxEVT_BUTTON, &TaskFrame::OnExportAll, this);
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

void TaskFrame::OnExportAll(wxCommandEvent &)
{
	if (!task_.IsFinished())
		return;
	wxArrayString arr;
	arr.Add("CSV");
	arr.Add("ISD");
	int r = wxGetSingleChoiceIndex("Choose a file format", "Export to", arr, this);
	if (r == -1) // cancelled
		return;
	wxDirDialog saveDirDialog(this,
							  "Target directory",
							  "", // TODO: defaultPath
							  wxDD_DEFAULT_STYLE|wxDD_DIR_MUST_EXIST);
	if (saveDirDialog.ShowModal() == wxID_CANCEL)
		return;
	ExportAllDialog *dialog = new ExportAllDialog(this, saveDirDialog.GetPath(), r);
	dialog->Show();
	dialog->Start();
}

void TaskFrame::OnClose(wxCloseEvent &)
{
	job_window_->gauge()->Stop();
	Destroy();
}

}
}
