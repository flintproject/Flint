/* -*- Mode: C++; tab-width: 4; indent-tabs-mode: t; c-basic-offset: 4 -*- vim:set ts=4 sw=4 sts=4 noet: */
#ifdef HAVE_CONFIG_H
#include "config.h"
#endif

#include "gui/view-frame.h"

#include <cstring>
#include <iostream>
#include <map>
#include <memory>
#include <string>

#define BOOST_FILESYSTEM_NO_DEPRECATED
#include <boost/filesystem.hpp>
#include <boost/process/io.hpp>
#ifdef _WIN32
#include <boost/process/windows.hpp>
#endif

#include "gui/app.h"
#include "gui/filename.h"
#include "gui/gnuplot.h"
#include "gui/job.h"
#include "gui/script-frame.h"
#include "gui/task-frame.h"
#include "gui/task.h"
#include "isdf/isdf.h"
#include "isdf/reader.h"

namespace flint {
namespace gui {

namespace {

enum {
	kColumnX,
	kColumnY1,
	kColumnY2,
	kColumnName,
	kColumnUuid,
	kColumnLabel,
	kColumnTitle
};

}

ViewFrame::ViewFrame(TaskFrame *parent, wxDataViewListCtrl &job_list)
	: wxFrame(parent, wxID_ANY, "View" /* FIXME */)
	, job_list_(job_list)
	, data_view_(new wxDataViewListCtrl(this, wxID_ANY))
	, legend_(new wxCheckBox(this, wxID_ANY, "Legend"))
	, log_x_(new wxCheckBox(this, wxID_ANY, "Log X"))
	, log_y1_(new wxCheckBox(this, wxID_ANY, "Log Y1"))
	, log_y2_(new wxCheckBox(this, wxID_ANY, "Log Y2"))
	, show_script_(new wxCheckBox(this, wxID_ANY, "Show gnuplot script"))
	, script_frame_(new ScriptFrame(this))
	, num_variables_(0)
	, skip_(0)
{
	data_view_->AppendToggleColumn("X", wxDATAVIEW_CELL_ACTIVATABLE, 30, wxALIGN_CENTER, wxDATAVIEW_COL_SORTABLE);
	data_view_->AppendToggleColumn("Y1", wxDATAVIEW_CELL_ACTIVATABLE, 30, wxALIGN_CENTER, wxDATAVIEW_COL_SORTABLE);
	data_view_->AppendToggleColumn("Y2", wxDATAVIEW_CELL_ACTIVATABLE, 30, wxALIGN_CENTER, wxDATAVIEW_COL_SORTABLE);
	data_view_->AppendTextColumn("Name", wxDATAVIEW_CELL_INERT, 80, wxALIGN_LEFT, wxDATAVIEW_COL_RESIZABLE|wxDATAVIEW_COL_SORTABLE);
	data_view_->AppendTextColumn("UUID", wxDATAVIEW_CELL_INERT, 300, wxALIGN_LEFT, wxDATAVIEW_COL_RESIZABLE|wxDATAVIEW_COL_SORTABLE);
	data_view_->AppendTextColumn("Label", wxDATAVIEW_CELL_INERT, 80, wxALIGN_LEFT, wxDATAVIEW_COL_RESIZABLE|wxDATAVIEW_COL_SORTABLE);
	data_view_->AppendTextColumn("Title", wxDATAVIEW_CELL_EDITABLE, 300, wxALIGN_LEFT, wxDATAVIEW_COL_RESIZABLE|wxDATAVIEW_COL_SORTABLE);

	LoadVariables();

	legend_->SetValue(true);

	auto hbox = new wxBoxSizer(wxHORIZONTAL);
	hbox->Add(legend_);
	hbox->Add(log_x_);
	hbox->Add(log_y1_);
	hbox->Add(log_y2_);
	hbox->Add(show_script_);
	auto vbox = new wxBoxSizer(wxVERTICAL);
	vbox->Add(data_view_, 1 /* vertically stretchable */, wxEXPAND /* horizontally stretchable */);
	vbox->Add(hbox);
	data_view_->SetMinSize(wxSize(600, 300));
	SetSizerAndFit(vbox);

	legend_->Bind(wxEVT_CHECKBOX, &ViewFrame::OnCheckBox, this);
	log_x_->Bind(wxEVT_CHECKBOX, &ViewFrame::OnCheckBox, this);
	log_y1_->Bind(wxEVT_CHECKBOX, &ViewFrame::OnCheckBox, this);
	log_y2_->Bind(wxEVT_CHECKBOX, &ViewFrame::OnCheckBox, this);
	show_script_->Bind(wxEVT_CHECKBOX, &ViewFrame::OnShowScript, this);
	Bind(wxEVT_DATAVIEW_ITEM_VALUE_CHANGED, &ViewFrame::OnItemValueChanged, this);
	Bind(wxEVT_CLOSE_WINDOW, &ViewFrame::OnClose, this);
}

ViewFrame::~ViewFrame() = default;

void ViewFrame::Plot()
{
	LineGraphOption option;
	option.legend = legend_->IsChecked();
	option.log_x = log_x_->IsChecked();
	option.log_y1 = log_y1_->IsChecked();
	option.log_y2 = log_y2_->IsChecked();

	wxDataViewItemArray arr;
	job_list_.GetSelections(arr);
	for (const auto &item : arr) {
		auto task_frame = wxDynamicCast(GetParent(), TaskFrame);
		Job job(task_frame->task(), job_list_.ItemToRow(item)+1);
		option.input_files.emplace(job.id, job.GetOutputFileName().GetFullPath());
	}

	option.x = -1;
	option.num_variables = num_variables_;
	option.skip = skip_;
	auto *store = data_view_->GetStore();
	wxVariant v;
	for (unsigned int i=0;i<store->GetItemCount();i++) {
		store->GetValueByRow(v, i, kColumnX);
		if (v.GetBool()) {
			option.x = static_cast<int>(i);
			continue;
		}
		store->GetValueByRow(v, i, kColumnY1);
		if (v.GetBool()) {
			store->GetValueByRow(v, i, kColumnTitle);
			option.y1.emplace(i, v.GetString());
			continue;
		}
		store->GetValueByRow(v, i, kColumnY2);
		if (v.GetBool()) {
			store->GetValueByRow(v, i, kColumnTitle);
			option.y2.emplace(i, v.GetString());
			continue;
		}
	}

	if (child_ && child_->valid() && child_->running()) {
		pipe_ << "clear" << std::endl;
		if (!pipe_) {
			wxLogError("failed to clear gnuplot window");
			return;
		}
	} else {
		auto gnuplot_executable = wxGetApp().GetGnuplotExecutable();
		if (!boost::filesystem::exists(gnuplot_executable)) {
			wxLogError("choose a valid gnuplot executable at Preferences");
			return;
		}
		child_.reset(new boost::process::child(gnuplot_executable,
											   "-persist",
											   "-raise",
#ifdef _WIN32
											   boost::process::windows::minimized_not_active,
#endif
											   boost::process::std_in < pipe_,
											   boost::process::std_out > boost::process::null,
											   boost::process::std_err > boost::process::null));
		if (!child_->valid()) {
			wxLogError("failed to call gnuplot");
			return;
		}
	}
	PlotLineGraph(option, dgo_.get(), pipe_);

	script_frame_->Clear();
	std::ostream os(script_frame_->GetTextCtrl());
	PlotLineGraph(option, dgo_.get(), os);
	os.flush();
}

void ViewFrame::UncheckShowScript()
{
	show_script_->SetValue(false);
}

void ViewFrame::OnCheckBox(wxCommandEvent &)
{
	Plot();
}

void ViewFrame::OnShowScript(wxCommandEvent &)
{
	script_frame_->Show(show_script_->IsChecked());
}

void ViewFrame::OnItemValueChanged(wxDataViewEvent &event)
{
	Unbind(wxEVT_DATAVIEW_ITEM_VALUE_CHANGED, &ViewFrame::OnItemValueChanged, this);
	int col = event.GetColumn();
	int row = data_view_->ItemToRow(event.GetItem());
	switch (col) {
	case kColumnX:
		if (data_view_->GetToggleValue(row, col)) {
			data_view_->SetToggleValue(false, row, kColumnY1);
			data_view_->SetToggleValue(false, row, kColumnY2);
			for (int i=0;i<data_view_->GetItemCount();i++) {
				if (i != row)
					data_view_->SetToggleValue(false, i, kColumnX);
			}
		}
		break;
	case kColumnY1:
		if (data_view_->GetToggleValue(row, col)) {
			data_view_->SetToggleValue(false, row, kColumnX);
			data_view_->SetToggleValue(false, row, kColumnY2);
		}
		break;
	case kColumnY2:
		if (data_view_->GetToggleValue(row, col)) {
			data_view_->SetToggleValue(false, row, kColumnX);
			data_view_->SetToggleValue(false, row, kColumnY1);
		}
		break;
	default:
		break;
	}
	Bind(wxEVT_DATAVIEW_ITEM_VALUE_CHANGED, &ViewFrame::OnItemValueChanged, this);

	Plot();
}

void ViewFrame::OnClose(wxCloseEvent &)
{
	script_frame_->Show(false);
	Show(false);
}

namespace {

class Handler {
public:
	Handler(wxDataViewListCtrl &data_view, bool dps_enabled);

	std::map<std::string, unsigned int> &m() {return m_;}

	void GetDescription(std::uint32_t i, std::uint32_t bytes, const char *d);

private:
	wxDataViewListCtrl &data_view_;
	bool dps_enabled_;
	std::map<std::string, unsigned int> m_;
};

Handler::Handler(wxDataViewListCtrl &data_view, bool dps_enabled)
	: data_view_(data_view)
	, dps_enabled_(dps_enabled)
{}

void Handler::GetDescription(std::uint32_t i, std::uint32_t bytes, const char *d)
{
	auto original_d = d;
	auto original_bytes = bytes;
	wxVector<wxVariant> data;
	data.push_back(i == 0);
	data.push_back(false);
	data.push_back(false);
	wxString uuid;
	wxString label;
	if (bytes > 36 && d[36] == ':') { // TODO
		uuid = wxString(d, 36u);
		d += 37;
		bytes -= 37;
	}
	for (std::uint32_t k=0;k<bytes;k++) {
		if (d[k] == '@') { // label found
			label = wxString(d+k+1, bytes-k-1);
			bytes = k;
		}
	}
	data.push_back(wxString(d, bytes));
	data.push_back(uuid);
	data.push_back(label);
	data.push_back(wxString(original_d, original_bytes));
	data_view_.AppendItem(data);

	if (dps_enabled_ && i > 0)
		m_.emplace(std::string(original_d, original_bytes), i);
}

struct DpsHandler {
	explicit DpsHandler(Handler &handler);

	void GetDescription(std::uint32_t i, std::uint32_t bytes, const char *d);

	std::map<std::string, unsigned int> &hm;
	std::map<unsigned int, unsigned int> m;
};

DpsHandler::DpsHandler(Handler &handler)
	: hm(handler.m())
{}

void DpsHandler::GetDescription(std::uint32_t i, std::uint32_t bytes, const char *d)
{
	if (i == 0)
		return;
	auto it = hm.find(std::string(d, bytes));
	if (it == hm.end())
		return;
	m.emplace(it->second, i);
}

}

bool ViewFrame::LoadVariables()
{
	auto task_frame = wxDynamicCast(GetParent(), TaskFrame);
	auto filename = task_frame->task().GetDirectoryName();
	filename.SetFullName("isdh");
	boost::filesystem::ifstream ifs(GetPathFromWxFileName(filename), std::ios::in|std::ios::binary);
	if (!ifs.is_open()) {
		wxLogError("failed to open %s", filename.GetFullPath());
		return false;
	}
	isdf::Reader reader;
	Handler handler(*data_view_, task_frame->task().HasObjective());
	bool b = reader.ReadHeader(&ifs) && reader.SkipComment(&ifs) && reader.ReadDescriptions(handler, &ifs);
	if (!b) {
		ifs.close();
		wxLogError("failed to load variables from %s", filename.GetFullPath());
		return false;
	}
	ifs.close();
	num_variables_ = reader.num_objs();
	skip_ = reader.GetDataOffset();

	if (task_frame->task().HasObjective()) {
		wxString dps_path = task_frame->task().GetDpsPath();
		ifs.open(GetPathFromWxString(dps_path), std::ios::in|std::ios::binary);
		if (!ifs.is_open()) {
			wxLogError("failed to open %s", dps_path);
			return false;
		}
		isdf::Reader dr;
		DpsHandler dh(handler);
		b = dr.ReadHeader(&ifs) && dr.SkipComment(&ifs) && dr.ReadDescriptions(dh, &ifs);
		if (!b) {
			ifs.close();
			wxLogError("failed to read %s", dps_path);
			return false;
		}
		ifs.close();
		std::unique_ptr<DpsGraphOption> dgo(new DpsGraphOption);
		dgo->dps_path = dps_path;
		dgo->num_variables = dr.num_objs();
		dgo->skip = dr.GetDataOffset();
		dgo->m = std::move(dh.m);
		dgo_ = std::move(dgo);
	}
	return true;
}

}
}
