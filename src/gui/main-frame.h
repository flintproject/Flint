/* -*- Mode: C++; tab-width: 4; indent-tabs-mode: t; c-basic-offset: 4 -*- vim:set ts=4 sw=4 sts=4 noet: */
#ifndef FLINT_GUI_MAIN_FRAME_H_
#define FLINT_GUI_MAIN_FRAME_H_

#include <wx/wx.h>
#include <wx/aui/aui.h>
#include <wx/filehistory.h>

namespace flint {
namespace gui {

class Document;

class MainFrame : public wxFrame
{
public:
	MainFrame();
	~MainFrame();

	bool OpenFile(const wxString &path);

	void OpenSubFrame(Document *document);

private:
	void OnOpen(wxCommandEvent &event);
	void OnRecentFile(wxCommandEvent &event);
	void OnClose(wxCommandEvent &event);
	void OnAbout(wxCommandEvent &event);
	void OnExit(wxCommandEvent &event);
	void OnRun(wxCommandEvent &event);
	void OnIdle(wxIdleEvent &event);

	wxAuiManager manager_;
	wxAuiNotebook *notebook_;
	wxFileHistory history_;

	int next_open_id_;
	int next_simulation_id_;
};

}
}

#endif
