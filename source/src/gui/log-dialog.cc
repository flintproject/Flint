/* -*- Mode: C++; tab-width: 4; indent-tabs-mode: t; c-basic-offset: 4 -*- vim:set ts=4 sw=4 sts=4 noet: */
#ifdef HAVE_CONFIG_H
#include "config.h"
#endif

#include "gui/log-dialog.h"

namespace flint {
namespace gui {

LogDialog::LogDialog(wxWindow *parent, const wxString &title)
	: wxDialog(parent, wxID_ANY, title, wxDefaultPosition, wxDefaultSize, wxCAPTION|wxCLOSE_BOX|wxMINIMIZE_BOX|wxMAXIMIZE_BOX|wxSYSTEM_MENU)
	, text_ctrl_(new wxTextCtrl(this, wxID_ANY, wxEmptyString, wxDefaultPosition, wxSize(400, 300), wxTE_MULTILINE|wxTE_READONLY))
{
	auto vbox = new wxBoxSizer(wxVERTICAL);
	vbox->Add(text_ctrl_, wxSizerFlags(1).Expand());
	auto button = new wxButton(this, wxID_ANY, "Close");
	vbox->Add(button,  wxSizerFlags(0).Right());
	SetSizerAndFit(vbox);

	button->Bind(wxEVT_BUTTON, &LogDialog::OnClose, this);
}

void LogDialog::OnClose(wxCommandEvent &)
{
	Show(false);
}

}
}
