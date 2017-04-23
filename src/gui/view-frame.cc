/* -*- Mode: C++; tab-width: 4; indent-tabs-mode: t; c-basic-offset: 4 -*- vim:set ts=4 sw=4 sts=4 noet: */
#ifdef HAVE_CONFIG_H
#include "config.h"
#endif

#include "gui/view-frame.h"

#include <cstring>
#include <fstream>
#include <iostream>
#include <vector>

#include "gui/gnuplot.h"
#include "gui/job.h"
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
	, num_variables_(0)
	, skip_(0)
	, fp_(nullptr)
{
	data_view_->AppendToggleColumn("X");
	data_view_->AppendToggleColumn("Y1");
	data_view_->AppendToggleColumn("Y2");
	data_view_->AppendTextColumn("Name");
	data_view_->AppendTextColumn("UUID");
	data_view_->AppendTextColumn("Label");

	LoadVariables();

	legend_->SetValue(true);

	auto hbox = new wxBoxSizer(wxHORIZONTAL);
	hbox->Add(legend_);
	hbox->Add(log_x_);
	hbox->Add(log_y1_);
	hbox->Add(log_y2_);
	auto vbox = new wxBoxSizer(wxVERTICAL);
	vbox->Add(data_view_, 1 /* vertically stretchable */, wxEXPAND /* horizontally stretchable */);
	vbox->Add(hbox);
	data_view_->SetMinSize(wxSize(600, 300));
	SetSizerAndFit(vbox);

	legend_->Bind(wxEVT_CHECKBOX, &ViewFrame::OnCheckBox, this);
	log_x_->Bind(wxEVT_CHECKBOX, &ViewFrame::OnCheckBox, this);
	log_y1_->Bind(wxEVT_CHECKBOX, &ViewFrame::OnCheckBox, this);
	log_y2_->Bind(wxEVT_CHECKBOX, &ViewFrame::OnCheckBox, this);
	Bind(wxEVT_DATAVIEW_ITEM_VALUE_CHANGED, &ViewFrame::OnItemValueChanged, this);
	Bind(wxEVT_CLOSE_WINDOW, &ViewFrame::OnClose, this);
}

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

#ifdef _WIN32
	// TODO
#elif defined(HAVE_POPEN)
	if (fp_) {
		if (std::fputs("clear\n", fp_) == EOF) {
			wxLogError("failed to clear gnuplot window");
			return;
		}
	} else {
		fp_ = popen("gnuplot", "w"); // FIXME
		if (!fp_) {
			wxLogError("failed to popen gnuplot");
			return;
		}
	}
	PlotLineGraph(option, fp_);
#endif
}

void ViewFrame::OnCheckBox(wxCommandEvent &)
{
	Plot();
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
	Show(false);
#ifdef _WIN32
	// TODO
#elif defined(HAVE_POPEN)
	if (fp_) {
		pclose(fp_);
		fp_ = nullptr;
	}
#endif
}

namespace {

class Handler {
public:
	Handler(wxDataViewListCtrl &data_view);

	void GetDescription(std::uint32_t i, std::uint32_t bytes, const char *d);

private:
	wxDataViewListCtrl &data_view_;
};

Handler::Handler(wxDataViewListCtrl &data_view)
	: data_view_(data_view)
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
}

}

bool ViewFrame::LoadVariables()
{
	auto task_frame = wxDynamicCast(GetParent(), TaskFrame);
	auto filename = task_frame->task().GetDirectoryName();
	filename.SetFullName("isdh");
	wxString full_path = filename.GetFullPath();
	std::ifstream ifs(full_path.c_str(), std::ios::in|std::ios::binary);
	if (!ifs.is_open()) {
		wxLogError("failed to open %s", full_path);
		return false;
	}
	isdf::Reader reader;
	Handler handler(*data_view_);
	bool b = reader.ReadHeader(&ifs) && reader.SkipComment(&ifs) && reader.ReadDescriptions(handler, &ifs);
	if (!b) {
		ifs.close();
		wxLogError("failed to load variables from %s", full_path);
		return false;
	}
	ifs.close();
	num_variables_ = reader.num_objs();
	skip_ = sizeof(isdf::ISDFHeader) + reader.num_bytes_comment() + reader.num_bytes_descs() + reader.num_bytes_units();
	return true;
}

}
}
