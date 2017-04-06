/* -*- Mode: C++; tab-width: 4; indent-tabs-mode: t; c-basic-offset: 4 -*- vim:set ts=4 sw=4 sts=4 noet: */
#ifndef FLINT_GUI_PHSP_EDITOR_DIALOG_H_
#define FLINT_GUI_PHSP_EDITOR_DIALOG_H_

#pragma GCC diagnostic push
#pragma GCC diagnostic ignored "-Wcast-qual"
#include <wx/wx.h>
#include <wx/choicebk.h>
#include <wx/dataview.h>
#pragma GCC diagnostic pop

namespace flint {
namespace gui {

class ParamTreeViewModel;
class ParamIntervalCtrl;
class ParamEnumCtrl;
class ParamGaussianCtrl;
class ParamUniformCtrl;

class PhspEditorDialog : public wxDialog {
public:
	explicit PhspEditorDialog(wxWindow *parent);

private:
	void OnApply(wxCommandEvent &event);
	void OnPlus(wxCommandEvent &event);
	void OnMinus(wxCommandEvent &event);

	void OnSelectionChanged(wxDataViewEvent &event);
	void OnChoicebookPageChanged(wxBookCtrlEvent &event);

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
