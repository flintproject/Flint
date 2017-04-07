/* -*- Mode: C++; tab-width: 4; indent-tabs-mode: t; c-basic-offset: 4 -*- vim:set ts=4 sw=4 sts=4 noet: */
#ifndef FLINT_GUI_SUB_FRAME_H_
#define FLINT_GUI_SUB_FRAME_H_

#include <string>
#include <unordered_map>
#include <vector>

#pragma GCC diagnostic push
#pragma GCC diagnostic ignored "-Wcast-qual"
#include <wx/dataview.h>
#include <wx/spinctrl.h>
#include <wx/wx.h>
#pragma GCC diagnostic pop

#include "gui/param-tree.h"

namespace flint {
namespace gui {

struct Configuration;
class Document;

class GeneralSetttingsWindow : public wxWindow
{
public:
	GeneralSetttingsWindow(wxWindow *parent, Document *doc);

	Document *doc() {return doc_;}

	void Write(Configuration *config) const;

private:
	Document *doc_;
	wxChoice *choice_method_;
	wxTextCtrl *text_length_;
	wxChoice *choice_length_;
	wxTextCtrl *text_step_;
	wxChoice *choice_step_;
	wxSpinCtrlDouble *spin_start_;
	wxChoice *choice_start_;
	wxSpinCtrl *spin_granularity_;
};

class OutputVariablesWindow : public wxWindow
{
public:
	OutputVariablesWindow(wxWindow *parent, const Document *doc);

	void Write(Configuration *config) const;

private:
	wxChoice *choice_pattern_;
	wxTextCtrl *text_pattern_;
	wxChoice *choice_column_;
};

class ParametersWindow : public wxWindow
{
public:
	ParametersWindow(wxWindow *parent, const Document *doc);

	/*
	 * Return modified entries only.
	 */
	std::unordered_map<int, std::string> GetParameters() const;

private:
	void OnEditParameterSet(wxCommandEvent &event);

	wxDataViewListCtrl *parameters_;
	std::vector<std::string> original_values_;
	ParamTree param_tree_;
};

}
}

#endif
