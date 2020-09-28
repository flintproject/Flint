/* -*- Mode: C++; tab-width: 4; indent-tabs-mode: t; c-basic-offset: 4 -*- vim:set ts=4 sw=4 sts=4 noet: */
#ifndef FLINT_GUI_LOG_DIALOG_H_
#define FLINT_GUI_LOG_DIALOG_H_

#pragma GCC diagnostic push
#pragma GCC diagnostic ignored "-Wcast-qual"
#include <wx/wx.h>
#pragma GCC diagnostic pop

namespace flint {
namespace gui {

class LogDialog : public wxDialog {
public:
	LogDialog(wxWindow *parent, const wxString &title);

	wxTextCtrl *text_ctrl() {return text_ctrl_;}

private:
	void OnClose(wxCommandEvent &event);

	wxTextCtrl *text_ctrl_;
};

}
}

#endif
