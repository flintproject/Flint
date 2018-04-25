/* -*- Mode: C++; tab-width: 4; indent-tabs-mode: t; c-basic-offset: 4 -*- vim:set ts=4 sw=4 sts=4 noet: */
#ifdef HAVE_CONFIG_H
#include "config.h"
#endif

#include "gui/script-frame.h"

#pragma GCC diagnostic push
#pragma GCC diagnostic ignored "-Wcast-qual"
#include <wx/sstream.h>
#include <wx/wfstream.h>
#pragma GCC diagnostic pop

#include "gui/view-frame.h"

namespace flint {
namespace gui {

ScriptFrame::ScriptFrame(ViewFrame *parent)
	: wxFrame(parent, wxID_ANY, "Gnuplot script")
	, script_text_(new wxTextCtrl(this, wxID_ANY, wxEmptyString, wxDefaultPosition, wxDefaultSize,
								  wxTE_MULTILINE|wxTE_READONLY))
	, save_(new wxButton(this, wxID_ANY, "Save"))
{
	script_text_->SetMinSize(wxSize(600, 150));

	auto box = new wxBoxSizer(wxVERTICAL);
	box->Add(script_text_, 1 /* vertically stretchable */, wxEXPAND /* horizontally stretchable */);
	box->Add(save_, 0, wxALIGN_RIGHT);
	SetSizerAndFit(box);

	save_->Bind(wxEVT_BUTTON, &ScriptFrame::OnSave, this);
	Bind(wxEVT_CLOSE_WINDOW, &ScriptFrame::OnClose, this);
}

ScriptFrame::~ScriptFrame() = default;

void ScriptFrame::Clear()
{
	script_text_->Clear();
}

wxTextCtrl *ScriptFrame::GetTextCtrl()
{
	return script_text_;
}

void ScriptFrame::OnClose(wxCloseEvent &)
{
	Show(false);
	wxStaticCast(GetParent(), ViewFrame)->UncheckShowScript();
}

void ScriptFrame::OnSave(wxCommandEvent &)
{
	wxFileDialog saveFileDialog(this,
								"Target file",
								wxGetHomeDir(),
								"plot.txt",
								"Text files (*.txt)|*.txt",
								wxFD_SAVE|wxFD_OVERWRITE_PROMPT);
	if (saveFileDialog.ShowModal() == wxID_CANCEL)
        return;
    wxFileOutputStream fos(saveFileDialog.GetPath());
    if (!fos) {
		wxMessageBox(wxString::Format("Failed to open %s", saveFileDialog.GetPath()),
					 "Failed to save",
					 wxOK|wxICON_ERROR,
					 this);
        return;
    }
	wxStringInputStream sis(script_text_->GetValue());
	if ( !fos.Write(sis) || !fos.Close() ) {
		wxMessageBox(wxString::Format("Failed to write into %s", saveFileDialog.GetPath()),
					 "Failed to save",
					 wxOK|wxICON_ERROR,
					 this);
        return;
	}
	wxMessageBox(wxString::Format("Saved as %s successfully", saveFileDialog.GetPath()),
				 "Saved",
				 wxOK|wxCENTRE,
				 this);
}

}
}
