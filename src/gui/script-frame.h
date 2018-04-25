/* -*- Mode: C++; tab-width: 4; indent-tabs-mode: t; c-basic-offset: 4 -*- vim:set ts=4 sw=4 sts=4 noet: */
#ifndef FLINT_GUI_SCRIPT_FRAME_H_
#define FLINT_GUI_SCRIPT_FRAME_H_

#pragma GCC diagnostic push
#pragma GCC diagnostic ignored "-Wcast-qual"
#include <wx/wx.h>
#pragma GCC diagnostic pop

namespace flint {
namespace gui {

class ViewFrame;

class ScriptFrame : public wxFrame {
public:
	explicit ScriptFrame(ViewFrame *parent);
	~ScriptFrame();

	void Clear();
	wxTextCtrl *GetTextCtrl();

private:
	void OnClose(wxCloseEvent &event);
	void OnSave(wxCommandEvent &event);

	wxTextCtrl *script_text_;
	wxButton *save_;
};

}
}

#endif
