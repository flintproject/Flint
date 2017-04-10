/* -*- Mode: C++; tab-width: 4; indent-tabs-mode: t; c-basic-offset: 4 -*- vim:set ts=4 sw=4 sts=4 noet: */
#ifndef FLINT_GUI_PHSP_EDITOR_DIALOG_H_
#define FLINT_GUI_PHSP_EDITOR_DIALOG_H_

#pragma GCC diagnostic push
#pragma GCC diagnostic ignored "-Wcast-qual"
#include <wx/wx.h>
#include <wx/choicebk.h>
#include <wx/dataview.h>
#include <wx/propgrid/propgrid.h>
#pragma GCC diagnostic pop

namespace flint {
namespace gui {

struct ParamTree;
class ParamTreeViewModel;
class ParamIntervalCtrl;
class ParamEnumCtrl;
class ParamGaussianCtrl;
class ParamUniformCtrl;

class PhspEditorDialog : public wxDialog {
public:
	PhspEditorDialog(wxWindow *parent, ParamTree &param_tree);

	void Save();

private:
	void OnApply(wxCommandEvent &event);
	void OnPlus(wxCommandEvent &event);
	void OnMinus(wxCommandEvent &event);

	void OnSelectionChanged(wxDataViewEvent &event);
	void OnChoicebookPageChanged(wxBookCtrlEvent &event);
	void OnIntervalPgChanged(wxPropertyGridEvent &event);
	void OnEnumText(wxCommandEvent &event);
	void OnGaussianPgChanged(wxPropertyGridEvent &event);
	void OnUniformPgChanged(wxPropertyGridEvent &event);

	void SpecifyValues(int selected);

	ParamTree &param_tree_;
	ParamTreeViewModel *model_;
	wxDataViewCtrl *tree_view_;
	wxChoicebook *book_;
	ParamIntervalCtrl *interval_ctrl_;
	ParamEnumCtrl *enum_ctrl_;
	ParamGaussianCtrl *gaussian_ctrl_;
	ParamUniformCtrl *uniform_ctrl_;
};

}
}

#endif
