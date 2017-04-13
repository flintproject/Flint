/* -*- Mode: C++; tab-width: 4; indent-tabs-mode: t; c-basic-offset: 4 -*- vim:set ts=4 sw=4 sts=4 noet: */
#ifdef HAVE_CONFIG_H
#include "config.h"
#endif

#include "gui/sub-window.h"

#include <cassert>
#include <sstream>

#pragma GCC diagnostic push
#pragma GCC diagnostic ignored "-Wcast-qual"
#include <wx/listctrl.h>
#pragma GCC diagnostic pop

#include "flint/numeric.h"
#include "gui/document.h"
#include "gui/phsp-editor-dialog.h"

namespace flint {
namespace gui {

GeneralSetttingsWindow::GeneralSetttingsWindow(wxWindow *parent, Document *doc)
	: wxWindow(parent, wxID_ANY)
	, doc_(doc)
	, choice_method_(new wxChoice(this,
								  wxID_ANY,
								  wxDefaultPosition,
								  wxDefaultSize,
								  doc_->choices_method()))
	, text_length_(new wxTextCtrl(this, wxID_ANY))
	, choice_length_(new wxChoice(this,
								  wxID_ANY,
								  wxDefaultPosition,
								  wxDefaultSize,
								  doc_->choices_time()))
	, text_step_(new wxTextCtrl(this, wxID_ANY))
	, choice_step_(new wxChoice(this,
								wxID_ANY,
								wxDefaultPosition,
								wxDefaultSize,
								doc_->choices_time()))
	, spin_start_(new wxSpinCtrlDouble(this))
	, choice_start_(new wxChoice(this,
								 wxID_ANY,
								 wxDefaultPosition,
								 wxDefaultSize,
								 doc_->choices_time()))
	, spin_granularity_(new wxSpinCtrl(this))
{
	// controls
	bool b = choice_method_->SetStringSelection(doc_->initial_config().method);
	assert(b);
	*text_length_ << doc_->initial_config().length;
	choice_length_->SetSelection(doc_->initial_config().length_unit);
	*text_step_ << doc_->initial_config().step;
	choice_step_->SetSelection(doc_->initial_config().step_unit);
	spin_start_->SetValue(doc_->initial_config().start);
	choice_start_->SetSelection(doc_->initial_config().start_unit);
	spin_granularity_->SetValue(doc_->initial_config().granularity);

	// sizers
	auto grid0 = new wxGridSizer(3, 3, 5, 5);
	grid0->Add(new wxStaticText(this, wxID_ANY, "Integration Method"), 0, wxEXPAND|wxALIGN_CENTER_VERTICAL);
	grid0->Add(choice_method_, 0, wxEXPAND|wxALIGN_CENTER_VERTICAL);
	grid0->Add(new wxStaticText(this, wxID_ANY, ""), 0, wxEXPAND|wxALIGN_CENTER_VERTICAL);
	grid0->Add(new wxStaticText(this, wxID_ANY, "Simulation Length"), 0, wxEXPAND|wxALIGN_CENTER_VERTICAL);
	grid0->Add(text_length_, 0, wxEXPAND|wxALIGN_CENTER_VERTICAL);
	grid0->Add(choice_length_, 0, wxEXPAND|wxALIGN_CENTER_VERTICAL);
	grid0->Add(new wxStaticText(this, wxID_ANY, "Simulation Time Step"), 0, wxEXPAND|wxALIGN_CENTER_VERTICAL);
	grid0->Add(text_step_, 0, wxEXPAND);
	grid0->Add(choice_step_, 0, wxEXPAND);
	auto vbox0 = new wxStaticBoxSizer(wxVERTICAL, this, "Numerical Integration");
	vbox0->Add(grid0, 0, wxEXPAND);
	auto grid1 = new wxGridSizer(2, 4, 5, 5);
	grid1->Add(new wxStaticText(this, wxID_ANY, "Starting from"), 0, wxEXPAND|wxALIGN_CENTER_VERTICAL);
	grid1->Add(spin_start_, 0, wxEXPAND|wxALIGN_CENTER_VERTICAL);
	grid1->Add(choice_start_, 0, wxEXPAND|wxALIGN_CENTER_VERTICAL);
	grid1->Add(new wxStaticText(this, wxID_ANY, ""), 0, wxEXPAND|wxALIGN_CENTER_VERTICAL);
	grid1->Add(new wxStaticText(this, wxID_ANY, "Data Sampling"), 0, wxEXPAND|wxALIGN_CENTER_VERTICAL);
	grid1->Add(new wxStaticText(this, wxID_ANY, "1 data per"), 0, wxALIGN_RIGHT|wxALIGN_CENTER_VERTICAL);
	grid1->Add(spin_granularity_, 0, wxEXPAND|wxALIGN_CENTER_VERTICAL);
	grid1->Add(new wxStaticText(this, wxID_ANY, "step(s)"), 0, wxEXPAND|wxALIGN_CENTER_VERTICAL);
	auto vbox1 = new wxStaticBoxSizer(wxVERTICAL, this, "Output");
	vbox1->Add(grid1, 0, wxEXPAND);
	auto topSizer = new wxBoxSizer(wxVERTICAL);
	topSizer->Add(vbox0, 0, wxEXPAND);
	topSizer->Add(vbox1, 0, wxEXPAND);
	SetSizerAndFit(topSizer);
}

void GeneralSetttingsWindow::Write(Configuration *config) const
{
	config->method = choice_method_->GetString(choice_method_->GetSelection());
	config->length = text_length_->GetValue();
	config->length_unit = choice_length_->GetSelection();
	config->step = text_step_->GetValue();
	config->step_unit = choice_step_->GetSelection();
	config->start = spin_start_->GetValue();
	config->start_unit = choice_step_->GetSelection();
	config->granularity = spin_granularity_->GetValue();
}


namespace {

const wxString choicesPattern[] = {"Regular expression", "Wildcard", "Fixed string"};

const wxString choicesColumn[] = {"Physical Quantity", "Module"};

}

OutputVariablesWindow::OutputVariablesWindow(wxWindow *parent, const Document *doc)
	: wxWindow(parent, wxID_ANY)
	, choice_pattern_(new wxChoice(this,
								   wxID_ANY,
								   wxDefaultPosition,
								   wxDefaultSize,
								   WXSIZEOF(choicesPattern),
								   choicesPattern))
	, text_pattern_(new wxTextCtrl(this, wxID_ANY))
	, choice_column_(new wxChoice(this,
								  wxID_ANY,
								  wxDefaultPosition,
								  wxDefaultSize,
								  WXSIZEOF(choicesColumn),
								  choicesColumn))
{
	long i;

	// controls
	auto availableVariables = new wxListView(this);
	availableVariables->AppendColumn("Physical Quantity");
	availableVariables->AppendColumn("Module");
	i = 0;
	for (auto &column : doc->var()) {
		availableVariables->InsertItem(i, column.name());
		availableVariables->SetItem(i, 1, column.track_name());
		++i;
	}

	auto vbox0 = new wxStaticBoxSizer(wxVERTICAL, this, "Enabled Variables");
	auto enabledVariables = new wxListView(vbox0->GetStaticBox());
	enabledVariables->AppendColumn("Physical Quantity");
	enabledVariables->AppendColumn("Module");
	i = 0;
	for (auto &column : doc->var()) {
		enabledVariables->InsertItem(i, column.name());
		enabledVariables->SetItem(i, 1, column.track_name());
		++i;
	}

	auto b = choice_pattern_->SetStringSelection(doc->initial_config().filter_pattern);
	assert(b);

	*text_pattern_ << doc->initial_config().filter_value;

	choice_column_->SetSelection(0);

	// sizers
	vbox0->Add(enabledVariables, 0, wxEXPAND);
	auto hbox0 = new wxBoxSizer(wxHORIZONTAL);
	hbox0->Add(new wxStaticText(this, wxID_ANY, "Filter Pattern:"), 0, wxALIGN_CENTER_VERTICAL);
	hbox0->Add(choice_pattern_, 0, wxALIGN_CENTER_VERTICAL);
	auto hbox1 = new wxBoxSizer(wxHORIZONTAL);
	hbox1->Add(new wxStaticText(this, wxID_ANY, "Filter Column:"), 0, wxALIGN_CENTER_VERTICAL);
	hbox1->Add(choice_column_, 0, wxALIGN_CENTER_VERTICAL);
	auto vbox = new wxBoxSizer(wxVERTICAL);
	vbox->Add(vbox0, 0, wxEXPAND);
	vbox->Add(hbox0, 0, wxEXPAND);
	vbox->Add(text_pattern_, 0, wxEXPAND);
	vbox->Add(hbox1, 0, wxEXPAND);
	auto topSizer = new wxBoxSizer(wxHORIZONTAL);
	topSizer->Add(availableVariables,
				  1, // horizontally stretchable
				  wxEXPAND);
	topSizer->Add(vbox,
				  1, // horizontally stretchable
				  wxEXPAND);
	SetSizerAndFit(topSizer);
}

void OutputVariablesWindow::Write(Configuration *config) const
{
	config->filter_pattern = choice_pattern_->GetString(choice_pattern_->GetSelection());
	config->filter_value = text_pattern_->GetValue();
	config->filter_column = choice_column_->GetSelection();
}


ParametersWindow::ParametersWindow(wxWindow *parent, const Document *doc)
	: wxWindow(parent, wxID_ANY)
	, parameters_(new wxDataViewListCtrl(this, wxID_ANY))
{
	// controls
	auto button = new wxButton(this, wxID_ANY, "Edit parameter set");
	button->Bind(wxEVT_BUTTON, &ParametersWindow::OnEditParameterSet, this);

	parameters_->AppendTextColumn("Module");
	parameters_->AppendTextColumn("PQ");
	parameters_->AppendTextColumn("Type");
	parameters_->AppendTextColumn("Expression", wxDATAVIEW_CELL_EDITABLE);

	std::ostringstream oss;
	RequestMaxNumOfDigitsForDouble(oss);
	wxVector<wxVariant> data;
	for (auto &column : doc->param()) {
		data.push_back(wxVariant(column.track_name()));
		data.push_back(wxVariant(column.name()));
		data.push_back(wxVariant((column.type() == lo::Type::S) ? "static-parameter" : "initial-value"));
		oss << doc->GetData(column.position());
		data.push_back(wxVariant(oss.str()));
		original_values_.push_back(oss.str());
		oss.str("");
		parameters_->AppendItem(data);
		data.clear();
	}

	// sizers
	auto hbox = new wxBoxSizer(wxHORIZONTAL);
	hbox->Add(new wxStaticText(this, wxID_ANY, ""), 1, wxEXPAND);
	hbox->Add(button, 0, wxFIXED_MINSIZE);
	auto topSizer = new wxBoxSizer(wxVERTICAL);
	topSizer->Add(hbox, 0, wxEXPAND);
	topSizer->Add(parameters_,
				  1, // vertically stretchable
				  wxEXPAND);
	SetSizerAndFit(topSizer);
}

void ParametersWindow::Write(Configuration *config) const
{
	config->parameters = GetParameters();
	config->param_map = GetParamMap();
}

std::unordered_map<int, std::string> ParametersWindow::GetParameters() const
{
	unsigned int n = parameters_->GetItemCount();
	assert(n == static_cast<unsigned int>(original_values_.size()));
	std::unordered_map<int, std::string> m;
	for (unsigned int i=0;i<n;i++) {
		auto value = parameters_->GetTextValue(i, 3);
		auto buf = value.utf8_str();
		std::string s(buf.data(), buf.length());
		if (s != original_values_.at(i))
			m.emplace(static_cast<int>(i), s);
	}
	return m;
}

ParamMap ParametersWindow::GetParamMap() const
{
	ParamMap m;
	for (auto p : param_tree_.pm)
		m.emplace(p.second, p.first);
	return m;
}

void ParametersWindow::OnEditParameterSet(wxCommandEvent &)
{
	PhspEditorDialog dialog(this, param_tree_);
	if (dialog.ShowModal() == wxID_OK)
		dialog.Save();
}

}
}
