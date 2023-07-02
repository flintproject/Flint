/* -*- Mode: C++; tab-width: 4; indent-tabs-mode: t; c-basic-offset: 4 -*- vim:set ts=4 sw=4 sts=4 noet: */
#ifdef HAVE_CONFIG_H
#include "config.h"
#endif

#include "gui/task-frame.h"

#include <cassert>
#include <cstdlib>
#include <iostream>
#include <vector>

#define BOOST_FILESYSTEM_NO_DEPRECATED
#include <boost/filesystem.hpp>
#define BOOST_DATE_TIME_NO_LIB
#include <boost/interprocess/file_mapping.hpp>

#include "db/read-only-driver.h"
#include "flint/error.h"
#include "gui/export-all-dialog.h"
#include "gui/filename.h"
#include "gui/job.h"
#include "gui/simulation.h"
#include "gui/task.h"
#include "gui/view-frame.h"
#include "isd2csv.h"
#include "sqlite3.h"

namespace flint {
namespace gui {

namespace {

enum {
	kColumnId,
	kColumnProgress,
	kColumnStatus,
	kColumnRss // optional
};

}

TaskFrame::TaskFrame(wxWindow *parent, const Task &task)
	: wxFrame(parent, wxID_ANY, wxString::Format("Job %d's Task %d", task.simulation->id, task.id))
	, task_(task)
	, data_view_(new wxDataViewListCtrl(this, wxID_ANY, wxDefaultPosition, wxDefaultSize, wxDV_MULTIPLE|wxDV_ROW_LINES))
	, export_(new wxButton(this, wxID_ANY, "Export"))
	, view_(new wxButton(this, wxID_ANY, "View"))
	, view_frame_(new ViewFrame(this, *data_view_))
	, timer_(this)
{
}

void TaskFrame::Start()
{
	unsigned int n = data_view_->GetItemCount();
	if (n == 0)
		return;
	if (!timer_.Start(1000+n*10))
		wxLogError("failed to start timer");
}

void TaskFrame::View()
{
	view_frame_->Centre();
	view_frame_->Show();
	view_frame_->Plot();
}

void TaskFrame::OnTimer(wxTimerEvent &)
{
	unsigned int n = data_view_->GetItemCount();
	for (unsigned int i=0;i<n;i++) {
		if (mr_.get_size() <= static_cast<size_t>(i+1))
			return;
		int progress = *(static_cast<char *>(mr_.get_address())+i+1);
		data_view_->SetValue(progress, i, kColumnProgress);
		data_view_->SetTextValue(wxString::Format("%d%%", progress), i, kColumnStatus);
		if (task_.HasObjective())
			data_view_->SetTextValue(GetRss(i+1), i, kColumnRss);
	}
	if (*(static_cast<char *>(mr_.get_address())) == 100 || task_.IsCanceled())
		timer_.Stop();
}

void TaskFrame::OnExport(wxCommandEvent &)
{
	if (data_view_->GetSelectedItemsCount() == 0)
		return;
	if (data_view_->GetSelectedItemsCount() == 1) {
		Job job(task_, data_view_->GetSelectedRow()+1);
		Export(job);
	} else {
		ExportAll();
	}
}

void TaskFrame::OnCancel(wxCommandEvent &event)
{
	auto *menu = wxDynamicCast(event.GetEventObject(), wxMenu);
	wxDataViewItem item(menu->GetClientData());
	if (!item.IsOk())
		return;
	int row = data_view_->ItemToRow(item);
	Job job(task_, row+1);
	job.RequestCancel();
}

void TaskFrame::OnItemContextMenu(wxDataViewEvent &event)
{
	auto item = event.GetItem();
	if (!item.IsOk())
		return;
	int row = data_view_->ItemToRow(item);
	Job job(task_, row+1);
	if (job.IsFinished())
		return;

	wxMenu menu;
	menu.SetClientData(item.GetID());
	menu.Append(wxID_ANY, "Cancel");
	menu.Bind(wxEVT_COMMAND_MENU_SELECTED, &TaskFrame::OnCancel, this);
	PopupMenu(&menu);
}

void TaskFrame::OnSelectionChanged(wxDataViewEvent &)
{
	bool b = data_view_->GetSelectedItemsCount() > 0;
	export_->Enable(b);
	view_->Enable(b);
}

void TaskFrame::OnView(wxCommandEvent &)
{
	if (data_view_->GetSelectedItemsCount() == 0)
		return;
	View();
}

void TaskFrame::OnClose(wxCloseEvent &)
{
	if (timer_.IsRunning())
		timer_.Stop();
	Destroy();
}

wxString TaskFrame::GetRss(int job_id)
{
	size_t offset = job_id * sizeof(double);
	assert(offset + sizeof(double) <= rss_mr_.get_size());
	double *p = reinterpret_cast<double *>(static_cast<char *>(rss_mr_.get_address())+ offset);
	return wxString::Format("%g", *p);
}

void TaskFrame::Export(const Job &job)
{
	if (!job.IsFinished())
		return;
	auto source_file = job.GetOutputFileName();
	if (!source_file.FileExists()) {
		ShowErrorOnExporting(wxString::Format("missing output file of job %d", job.id));
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
								source_file.GetName() + ((r == 0) ? ".csv" : ".isd"),
								(r == 0) ? "CSV files (*.csv)|*.csv" : "ISDF files (*.isd)|*.isd",
								wxFD_SAVE|wxFD_OVERWRITE_PROMPT);
	if (saveFileDialog.ShowModal() == wxID_CANCEL)
		return;
	auto target_path = saveFileDialog.GetPath();
	if (r == 0) { // CSV
		boost::filesystem::ifstream ifs(GetPathFromWxFileName(source_file), std::ios::in|std::ios::binary);
		if (!ifs.is_open()) {
			ShowErrorOnExporting(wxString::Format("failed to open %s", source_file.GetFullPath()));
			return;
		}
		boost::filesystem::ofstream ofs(GetPathFromWxString(target_path), std::ios::out|std::ios::binary);
		if (!ofs.is_open()) {
			ifs.close();
			ShowErrorOnExporting(wxString::Format("failed to open %s", target_path));
			return;
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
			ShowErrorOnExporting(wxString::Format("failed to export %s: %s", target_path, ec.Get()));
			return;
		}
	} else { // ISD
		if (!wxCopyFile(source_file.GetFullPath(), target_path)) {
			ShowErrorOnExporting(wxString::Format("failed to export %s", target_path));
			return;
		}
	}
	wxMessageBox(wxString::Format("exported to %s successfully", target_path), "Exported");
}

void TaskFrame::ExportAll()
{
	wxDataViewItemArray selected;
	data_view_->GetSelections(selected);
	assert(!selected.empty());

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

	std::vector<int> indice;
	for (const auto &item : selected)
		indice.push_back(data_view_->ItemToRow(item));
	ExportAllDialog *dialog = new ExportAllDialog(this, saveDirDialog.GetPath(), r, std::move(indice));
	dialog->Show();
	dialog->Start();
}

int TaskFrame::AddParameterSample(int argc, char **argv, char **names)
{
	assert(argc > 1);

	if (data_view_->GetItemCount() == 0) {
		for (int i=1;i<argc;i++)
			data_view_->AppendTextColumn(names[i], wxDATAVIEW_CELL_INERT, 50, wxALIGN_LEFT, wxDATAVIEW_COL_RESIZABLE|wxDATAVIEW_COL_SORTABLE);
	}

	int id = std::atoi(argv[0]);
	assert(id > 0);
	if (mr_.get_size() <= static_cast<size_t>(id))
		return 1;
	int progress = *(static_cast<char *>(mr_.get_address())+id);
	wxString param;
	wxVector<wxVariant> data;
	data.push_back(wxString::Format("%d", id));
	data.push_back(progress);
	data.push_back(wxString::Format("%d%%", progress));
	if (task_.HasObjective())
		data.push_back(GetRss(id));
	for (int i=1;i<argc;i++)
		data.push_back(argv[i]);
	data_view_->AppendItem(data);
	return 0;
}

namespace {

int Process(void *data, int argc, char **argv, char **names)
{
	auto *task_frame = static_cast<TaskFrame *>(data);
	return task_frame->AddParameterSample(argc, argv, names);
}

}

bool TaskFrame::LoadItems()
{
	data_view_->AppendTextColumn("ID", wxDATAVIEW_CELL_INERT, 80, wxALIGN_RIGHT, wxDATAVIEW_COL_SORTABLE);
	data_view_->AppendProgressColumn("Progress", wxDATAVIEW_CELL_INERT, 100, wxALIGN_CENTER, wxDATAVIEW_COL_SORTABLE);
	data_view_->AppendTextColumn("Status", wxDATAVIEW_CELL_INERT, 50, wxALIGN_RIGHT, wxDATAVIEW_COL_SORTABLE);
	if (task_.HasObjective()) {
		data_view_->AppendTextColumn("RSS", wxDATAVIEW_CELL_INERT, 50, wxALIGN_RIGHT, wxDATAVIEW_COL_RESIZABLE|wxDATAVIEW_COL_SORTABLE);
		try {
			auto rss_filename = task_.GetRssFileName();
			boost::interprocess::file_mapping rss_fm(GetFnStrFromWxFileName(rss_filename).c_str(),
													 boost::interprocess::read_only);
			rss_mr_ = boost::interprocess::mapped_region(rss_fm, boost::interprocess::read_only);
		} catch (const boost::interprocess::interprocess_exception &e) {
			wxLogError("failed to read RSS: %s", e.what());
			return false;
		}
	}

	try {
		auto filename = task_.GetProgressFileName();
		boost::interprocess::file_mapping fm(GetFnStrFromWxFileName(filename).c_str(),
											 boost::interprocess::read_only);
		mr_ = boost::interprocess::mapped_region(fm, boost::interprocess::read_only);
	} catch (const boost::interprocess::interprocess_exception &e) {
		wxLogError("failed to read progress: %s", e.what());
		return false;
	}

	// FIXME: the database can be busy
	auto filename = task_.GetDirectoryName();
	filename.SetFullName("task.db");
	auto driver = db::ReadOnlyDriver::Create(GetPathFromWxFileName(filename));
	if (!driver->db()) {
		wxLogError("failed to open task database");
		return false;
	}
	char *em;
	int e = sqlite3_exec(driver->db(), "SELECT rowid, * FROM parameter_samples", &Process, this, &em);
	if (e != SQLITE_OK)
		if (e != SQLITE_ABORT) {
			wxLogError(em);
			return false;
		}

	if (data_view_->GetItemCount() > 0)
		data_view_->SelectRow(0);

	auto hbox = new wxBoxSizer(wxHORIZONTAL);
	hbox->Add(export_);
	hbox->Add(view_);
	auto vbox = new wxBoxSizer(wxVERTICAL);
	vbox->Add(data_view_, 1 /* vertically stretchable */, wxEXPAND /* horizontally stretchable */);
	vbox->Add(hbox, 0, wxALIGN_RIGHT);
	data_view_->SetMinSize(wxSize(400, 200));
	SetSizerAndFit(vbox);

	export_->Bind(wxEVT_BUTTON, &TaskFrame::OnExport, this);
	view_->Bind(wxEVT_BUTTON, &TaskFrame::OnView, this);
	Bind(wxEVT_TIMER, &TaskFrame::OnTimer, this);
	Bind(wxEVT_DATAVIEW_ITEM_CONTEXT_MENU, &TaskFrame::OnItemContextMenu, this);
	Bind(wxEVT_DATAVIEW_SELECTION_CHANGED, &TaskFrame::OnSelectionChanged, this);
	Bind(wxEVT_CLOSE_WINDOW, &TaskFrame::OnClose, this);
	return true;
}

void TaskFrame::ShowErrorOnExporting(const wxString &message)
{
	wxMessageBox(message, "Error on exporting", wxOK|wxICON_ERROR, this);
}

}
}
