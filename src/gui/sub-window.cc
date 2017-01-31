/* -*- Mode: C++; tab-width: 4; indent-tabs-mode: t; c-basic-offset: 4 -*- vim:set ts=4 sw=4 sts=4 noet: */
#ifdef HAVE_CONFIG_H
#include "config.h"
#endif

#include "gui/sub-window.h"

#include <cassert>
#include <sstream>

#include <wx/dataview.h>
#include <wx/listctrl.h>
#include <wx/spinctrl.h>

#include "flint/numeric.h"

namespace flint {
namespace gui {

GeneralSetttingsWindow::GeneralSetttingsWindow(wxWindow *parent, Document *doc)
	: wxWindow(parent, wxID_ANY)
	, doc_(doc)
{
	bool b;
	auto panel = new wxPanel(this);

	// controls
	auto choiceMethod = new wxChoice(panel,
									 wxID_ANY,
									 wxDefaultPosition,
									 wxDefaultSize,
									 doc_->choices_method());
	b = choiceMethod->SetStringSelection(doc_->initial_config().method);
	assert(b);

	auto textLength = new wxTextCtrl(panel, wxID_ANY);
	*textLength << doc_->initial_config().length;

	auto choiceLength = new wxChoice(panel,
									 wxID_ANY,
									 wxDefaultPosition,
									 wxDefaultSize,
									 doc_->choices_time());
	choiceLength->SetSelection(doc_->initial_config().length_unit);

	auto textStep = new wxTextCtrl(panel, wxID_ANY);
	*textStep << doc_->initial_config().step;

	auto choiceStep = new wxChoice(panel,
									 wxID_ANY,
									 wxDefaultPosition,
									 wxDefaultSize,
									 doc_->choices_time());
	choiceStep->SetSelection(doc_->initial_config().step_unit);

	auto spinStart = new wxSpinCtrlDouble(panel);
	spinStart->SetValue(doc_->initial_config().start);

	auto choiceStart = new wxChoice(panel,
									 wxID_ANY,
									 wxDefaultPosition,
									 wxDefaultSize,
									 doc_->choices_time());
	choiceStart->SetSelection(doc_->initial_config().start_unit);

	auto sampling = new wxSpinCtrl(panel);
	sampling->SetValue(doc_->initial_config().granularity);

	// sizers
	auto grid0 = new wxGridSizer(3, 3, 5, 5);
	grid0->Add(new wxStaticText(panel, wxID_ANY, "Integration Method"), 0, wxEXPAND|wxALIGN_CENTER_VERTICAL);
	grid0->Add(choiceMethod, 0, wxEXPAND|wxALIGN_CENTER_VERTICAL);
	grid0->Add(new wxStaticText(panel, wxID_ANY, ""), 0, wxEXPAND|wxALIGN_CENTER_VERTICAL);
	grid0->Add(new wxStaticText(panel, wxID_ANY, "Simulation Length"), 0, wxEXPAND|wxALIGN_CENTER_VERTICAL);
	grid0->Add(textLength, 0, wxEXPAND|wxALIGN_CENTER_VERTICAL);
	grid0->Add(choiceLength, 0, wxEXPAND|wxALIGN_CENTER_VERTICAL);
	grid0->Add(new wxStaticText(panel, wxID_ANY, "Simulation Time Step"), 0, wxEXPAND|wxALIGN_CENTER_VERTICAL);
	grid0->Add(textStep, 0, wxEXPAND);
	grid0->Add(choiceStep, 0, wxEXPAND);
	auto vbox0 = new wxStaticBoxSizer(wxVERTICAL, panel, "Numerical Integration");
	vbox0->Add(grid0, 1, wxEXPAND);
	auto grid1 = new wxGridSizer(2, 4, 5, 5);
	grid1->Add(new wxStaticText(panel, wxID_ANY, "Starting from"), 0, wxEXPAND|wxALIGN_CENTER_VERTICAL);
	grid1->Add(spinStart, 0, wxEXPAND|wxALIGN_CENTER_VERTICAL);
	grid1->Add(choiceStart, 0, wxEXPAND|wxALIGN_CENTER_VERTICAL);
	grid1->Add(new wxStaticText(panel, wxID_ANY, ""), 0, wxEXPAND|wxALIGN_CENTER_VERTICAL);
	grid1->Add(new wxStaticText(panel, wxID_ANY, "Data Sampling"), 0, wxEXPAND|wxALIGN_CENTER_VERTICAL);
	grid1->Add(new wxStaticText(panel, wxID_ANY, "1 data per"), 0, wxALIGN_RIGHT|wxALIGN_CENTER_VERTICAL);
	grid1->Add(sampling, 0, wxEXPAND|wxALIGN_CENTER_VERTICAL);
	grid1->Add(new wxStaticText(panel, wxID_ANY, "step(s)"), 0, wxEXPAND|wxALIGN_CENTER_VERTICAL);
	auto vbox1 = new wxStaticBoxSizer(wxVERTICAL, panel, "Output");
	vbox1->Add(grid1, 1, wxEXPAND);
	auto topSizer = new wxBoxSizer(wxVERTICAL);
	topSizer->Add(vbox0, 0, wxEXPAND);
	topSizer->Add(vbox1, 0, wxEXPAND);
	panel->SetSizerAndFit(topSizer);
}

OutputVariablesWindow::OutputVariablesWindow(wxWindow *parent, Document *doc)
	: wxWindow(parent, wxID_ANY)
	, doc_(doc)
{
	bool b;
	long i;
	auto panel = new wxPanel(this);

	// controls
	auto availableVariables = new wxListView(panel);
	availableVariables->AppendColumn("Physical Quantity");
	availableVariables->AppendColumn("Module");
	i = 0;
	for (auto &column : doc->var()) {
		availableVariables->InsertItem(i, column.name());
		availableVariables->SetItem(i, 1, column.track_name());
		++i;
	}

	auto enabledVariables = new wxListView(panel);
	enabledVariables->AppendColumn("Physical Quantity");
	enabledVariables->AppendColumn("Module");
	i = 0;
	for (auto &column : doc->var()) {
		enabledVariables->InsertItem(i, column.name());
		enabledVariables->SetItem(i, 1, column.track_name());
		++i;
	}

	const wxString choicesPattern[] = {"Regular expression", "Wildcard", "Fixed string"};
	auto choicePattern = new wxChoice(panel,
									 wxID_ANY,
									 wxDefaultPosition,
									 wxDefaultSize,
									 WXSIZEOF(choicesPattern),
									 choicesPattern);
	b = choicePattern->SetStringSelection(doc->initial_config().filter_pattern);
	assert(b);

	auto textPattern = new wxTextCtrl(panel, wxID_ANY);
	*textPattern << doc->initial_config().filter_value;

	const wxString choicesColumn[] = {"Physical Quantity", "Module"};
	auto choiceColumn = new wxChoice(panel,
									 wxID_ANY,
									 wxDefaultPosition,
									 wxDefaultSize,
									 WXSIZEOF(choicesColumn),
									 choicesColumn);
	choiceColumn->SetSelection(0);

	// sizers
	auto vbox0 = new wxStaticBoxSizer(wxVERTICAL, panel, "Enabled Variables");
	vbox0->Add(enabledVariables, 0, wxEXPAND);
	auto hbox0 = new wxBoxSizer(wxHORIZONTAL);
	hbox0->Add(new wxStaticText(panel, wxID_ANY, "Filter Pattern:"), 0, wxALIGN_CENTER_VERTICAL);
	hbox0->Add(choicePattern, 0, wxALIGN_CENTER_VERTICAL);
	auto hbox1 = new wxBoxSizer(wxHORIZONTAL);
	hbox1->Add(new wxStaticText(panel, wxID_ANY, "Filter Column:"), 0, wxALIGN_CENTER_VERTICAL);
	hbox1->Add(choiceColumn, 0, wxALIGN_CENTER_VERTICAL);
	auto vbox = new wxBoxSizer(wxVERTICAL);
	vbox->Add(vbox0, 0, wxEXPAND);
	vbox->Add(hbox0, 0, wxEXPAND);
	vbox->Add(textPattern, 0, wxEXPAND);
	vbox->Add(hbox1, 0, wxEXPAND);
	auto topSizer = new wxBoxSizer(wxHORIZONTAL);
	topSizer->Add(availableVariables, 0, wxEXPAND);
	topSizer->Add(vbox, 0, wxEXPAND);
	panel->SetSizerAndFit(topSizer);
}

ParametersWindow::ParametersWindow(wxWindow *parent, Document *doc)
	: wxWindow(parent, wxID_ANY)
	, doc_(doc)
{
	// controls
	auto button = new wxButton(this, wxID_ANY, "Define value set");

	auto parameters = new wxDataViewListCtrl(this, wxID_ANY);
	parameters->AppendTextColumn("Module");
	parameters->AppendTextColumn("PQ");
	parameters->AppendTextColumn("Type");
	parameters->AppendTextColumn("Expression", wxDATAVIEW_CELL_EDITABLE);

	std::ostringstream oss;
	RequestMaxNumOfDigitsForDouble(oss);
	wxVector<wxVariant> data;
	for (auto &column : doc->param()) {
		data.push_back(wxVariant(column.track_name()));
		data.push_back(wxVariant(column.name()));
		data.push_back(wxVariant((column.type() == lo::Type::S) ? "static-parameter" : "initial-value"));
		oss << doc->GetData(column.position());
		data.push_back(wxVariant(oss.str())); // FIXME
		oss.str("");
		parameters->AppendItem(data);
		data.clear();
	}

	// sizers
	auto hbox = new wxBoxSizer(wxHORIZONTAL);
	hbox->Add(new wxStaticText(this, wxID_ANY, ""), 1, wxEXPAND);
	hbox->Add(button, 0, wxFIXED_MINSIZE);
	auto topSizer = new wxBoxSizer(wxVERTICAL);
	topSizer->Add(hbox, 0, wxEXPAND);
	topSizer->Add(parameters,
				  1, // vertically stretchable
				  wxEXPAND);
	SetSizerAndFit(topSizer);
}

}
}
