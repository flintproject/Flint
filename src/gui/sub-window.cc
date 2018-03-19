/* -*- Mode: C++; tab-width: 4; indent-tabs-mode: t; c-basic-offset: 4 -*- vim:set ts=4 sw=4 sts=4 noet: */
#ifdef HAVE_CONFIG_H
#include "config.h"
#endif

#include "gui/sub-window.h"

#include <array>
#include <cassert>
#include <limits>
#include <memory>
#include <sstream>

#pragma GCC diagnostic push
#pragma GCC diagnostic ignored "-Wcast-qual"
#include <wx/valnum.h>
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
	, text_length_(new wxTextCtrl(this,
								  wxID_ANY,
								  wxEmptyString,
								  wxDefaultPosition,
								  wxDefaultSize,
								  0, // style
								  wxFloatingPointValidator<double>(nullptr, wxNUM_VAL_ZERO_AS_BLANK|wxNUM_VAL_NO_TRAILING_ZEROES)))
	, choice_length_(new wxChoice(this,
								  wxID_ANY,
								  wxDefaultPosition,
								  wxDefaultSize,
								  doc_->choices_time()))
	, text_step_(new wxTextCtrl(this,
								wxID_ANY,
								wxEmptyString,
								wxDefaultPosition,
								wxDefaultSize,
								0, // style
								wxFloatingPointValidator<double>(nullptr, wxNUM_VAL_ZERO_AS_BLANK|wxNUM_VAL_NO_TRAILING_ZEROES)))
	, choice_step_(new wxChoice(this,
								wxID_ANY,
								wxDefaultPosition,
								wxDefaultSize,
								doc_->choices_time()))
	, text_start_(new wxTextCtrl(this,
								 wxID_ANY,
								 wxEmptyString,
								 wxDefaultPosition,
								 wxDefaultSize,
								 0, // style
								 wxFloatingPointValidator<double>(nullptr, wxNUM_VAL_ZERO_AS_BLANK|wxNUM_VAL_NO_TRAILING_ZEROES)))
	, choice_start_(new wxChoice(this,
								 wxID_ANY,
								 wxDefaultPosition,
								 wxDefaultSize,
								 doc_->choices_time()))
	, spin_granularity_(new wxSpinCtrl(this,
									   wxID_ANY,
									   wxEmptyString,
									   wxDefaultPosition,
									   wxDefaultSize,
									   wxSP_ARROW_KEYS,
									   1, // minimal value
									   std::numeric_limits<int>::max())) // maximal value
{
	// controls
	bool b = choice_method_->SetStringSelection(doc_->initial_config().method);
	assert(b);
	*text_length_ << doc_->initial_config().length;
	choice_length_->SetSelection(doc_->initial_config().length_unit);
	*text_step_ << doc_->initial_config().step;
	choice_step_->SetSelection(doc_->initial_config().step_unit);
	*text_start_ << doc_->initial_config().start;
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
	grid1->Add(text_start_, 0, wxEXPAND|wxALIGN_CENTER_VERTICAL);
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
	if (!text_start_->GetValue().ToDouble(&config->start))
		config->start = 0;
	config->start_unit = choice_step_->GetSelection();
	config->granularity = spin_granularity_->GetValue();
}


namespace {

const wxString choicesPattern[] = {"Regular expression", "Wildcard", "Fixed string"};

const std::array<wxString, 2> choicesColumnsCellml = {"Variable", "Component"};
const std::array<wxString, 2> choicesColumnsPhml = {"Physical Quantity", "Module"};
const std::array<wxString, 2> choicesColumnsSbml = {"Species/Reaction", "-"};

const std::array<wxString, 2> &GetChoices(const Document *doc)
{
	switch (doc->format()) {
	case file::kCellml:
		return choicesColumnsCellml;
	case file::kSbml:
		return choicesColumnsSbml;
	default:
		return choicesColumnsPhml;
	}
}

}

OutputVariablesWindow::OutputVariablesWindow(wxWindow *parent, const Document *doc)
	: wxWindow(parent, wxID_ANY)
	, doc_(doc)
	, enabled_variables_(new wxListView(this))
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
								  2,
								  GetChoices(doc).data()))
{
	// controls
	for (auto &column : GetChoices(doc))
		enabled_variables_->AppendColumn(column);
	Choose(doc->var());
	enabled_variables_->SetColumnWidth(0, wxLIST_AUTOSIZE_USEHEADER);
	enabled_variables_->SetColumnWidth(1, wxLIST_AUTOSIZE);

	auto b = choice_pattern_->SetStringSelection(doc->initial_config().filter_pattern);
	assert(b);

	*text_pattern_ << doc->initial_config().filter_value;

	choice_column_->SetSelection(0);

	// sizers
	auto hbox0 = new wxBoxSizer(wxHORIZONTAL);
	hbox0->Add(new wxStaticText(this, wxID_ANY, "Filter Pattern:"), 0, wxALIGN_CENTER_VERTICAL);
	hbox0->Add(choice_pattern_, 0, wxALIGN_CENTER_VERTICAL);
	auto hbox1 = new wxBoxSizer(wxHORIZONTAL);
	hbox1->Add(new wxStaticText(this, wxID_ANY, "Filter Column:"), 0, wxALIGN_CENTER_VERTICAL);
	hbox1->Add(choice_column_, 0, wxALIGN_CENTER_VERTICAL);
	auto vbox = new wxBoxSizer(wxVERTICAL);
	vbox->Add(enabled_variables_, 1 /* vertically stretchable */, wxEXPAND);
	vbox->Add(hbox0, 0, wxEXPAND);
	vbox->Add(text_pattern_, 0, wxEXPAND);
	vbox->Add(hbox1, 0, wxEXPAND);
	SetSizerAndFit(vbox);

	choice_pattern_->Bind(wxEVT_CHOICE, &OutputVariablesWindow::OnChoice, this);
	text_pattern_->Bind(wxEVT_TEXT, &OutputVariablesWindow::OnChoice, this);
	choice_column_->Bind(wxEVT_CHOICE, &OutputVariablesWindow::OnChoice, this);
}

void OutputVariablesWindow::Write(Configuration *config) const
{
	config->filter_pattern = choice_pattern_->GetString(choice_pattern_->GetSelection());
	config->filter_value = text_pattern_->GetValue();
	config->filter_column = choice_column_->GetSelection();
}

void OutputVariablesWindow::OnChoice(wxCommandEvent &)
{
	// TODO: as this procedure takes some time, it would be nicer
	// to show an indicator until finished.
	std::unique_ptr<Configuration> config(new Configuration(doc_->initial_config()));
	Write(config.get());
	std::vector<lo::Column> v;
	config->GetOutputVariables(doc_, &v);
	enabled_variables_->DeleteAllItems();
	Choose(v);
}

void OutputVariablesWindow::Choose(const std::vector<lo::Column> &v)
{
	long i = 0;
	for (auto &column : v) {
		enabled_variables_->InsertItem(i, column.name());
		enabled_variables_->SetItem(i, 1, column.track_name());
		++i;
	}
}

namespace {

const std::array<wxString, 2> parameterColumnsCellml = {"Component", "Variable"};
const std::array<wxString, 2> parameterColumnsPhml = {"Module", "PQ"};
const std::array<wxString, 2> parameterColumnsSbml = {"-", "Name"};

const std::array<wxString, 2> &GetParameterColumns(const Document *doc)
{
	switch (doc->format()) {
	case file::kCellml:
		return parameterColumnsCellml;
	case file::kSbml:
		return parameterColumnsSbml;
	default:
		return parameterColumnsPhml;
	}
}

}

ParametersWindow::ParametersWindow(wxWindow *parent, const Document *doc)
	: wxWindow(parent, wxID_ANY)
	, parameters_(new wxDataViewListCtrl(this, wxID_ANY))
{
	// controls
	auto button = new wxButton(this, wxID_ANY, "Edit parameter set");
	button->Bind(wxEVT_BUTTON, &ParametersWindow::OnEditParameterSet, this);

	for (auto &column : GetParameterColumns(doc))
		parameters_->AppendTextColumn(column, wxDATAVIEW_CELL_INERT, wxDVC_DEFAULT_WIDTH)->SetSortable(true);
	parameters_->AppendTextColumn("Type")->SetSortable(true);
	parameters_->AppendTextColumn("Expression", wxDATAVIEW_CELL_EDITABLE)->SetSortable(true);

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

ObjectiveWindow::ObjectiveWindow(wxWindow *parent)
	: wxWindow(parent, wxID_ANY)
	, check_box_(new wxCheckBox(this, wxID_ANY, "Enable parameter fitting by the method of least-squares"))
	, file_picker_(new wxFilePickerCtrl(this, wxID_ANY))
{
	auto topSizer = new wxBoxSizer(wxVERTICAL);
	topSizer->Add(check_box_);
	topSizer->Add(file_picker_);
	SetSizerAndFit(topSizer);

	// controls
	file_picker_->Disable();

	// signals
	check_box_->Bind(wxEVT_CHECKBOX, &ObjectiveWindow::OnCheck, this);
}

void ObjectiveWindow::Write(Configuration *config) const
{
	if (check_box_->IsChecked())
		config->dps_path = file_picker_->GetPath();
}

void ObjectiveWindow::OnCheck(wxCommandEvent &)
{
	file_picker_->Enable(check_box_->IsChecked());
}

}
}
