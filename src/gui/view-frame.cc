/* -*- Mode: C++; tab-width: 4; indent-tabs-mode: t; c-basic-offset: 4 -*- vim:set ts=4 sw=4 sts=4 noet: */
#ifdef HAVE_CONFIG_H
#include "config.h"
#endif

#include "gui/view-frame.h"

#include <cstring>
#include <fstream>
#include <iostream>

#include "gui/task-frame.h"
#include "gui/task.h"
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
	kColumnLabel
};

}

ViewFrame::ViewFrame(TaskFrame *parent)
	: wxFrame(parent, wxID_ANY, "View" /* FIXME */)
	, data_view_(new wxDataViewListCtrl(this, wxID_ANY))
{
	data_view_->AppendToggleColumn("X");
	data_view_->AppendToggleColumn("Y1");
	data_view_->AppendToggleColumn("Y2");
	data_view_->AppendTextColumn("Name");
	data_view_->AppendTextColumn("UUID");
	data_view_->AppendTextColumn("Label");

	LoadVariables();

	auto vbox = new wxBoxSizer(wxVERTICAL);
	vbox->Add(data_view_, 1 /* vertically stretchable */, wxEXPAND /* horizontally stretchable */);
	vbox->Add(new wxButton(this, wxID_ANY, "Plot"), 0, wxEXPAND /* horizontally stretchable */);
	data_view_->SetMinSize(wxSize(600, 300));
	SetSizerAndFit(vbox);

	Bind(wxEVT_DATAVIEW_ITEM_VALUE_CHANGED, &ViewFrame::OnItemValueChanged, this);
	Bind(wxEVT_CLOSE_WINDOW, &ViewFrame::OnClose, this);
}

void ViewFrame::OnItemValueChanged(wxDataViewEvent &event)
{
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
}

void ViewFrame::OnClose(wxCloseEvent &)
{
	Show(false);
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
	if (!b)
		wxLogError("failed to load variables from %s", full_path);
	ifs.close();
	return b;
}

}
}
