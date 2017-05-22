/* -*- Mode: C++; tab-width: 4; indent-tabs-mode: t; c-basic-offset: 4 -*- vim:set ts=4 sw=4 sts=4 noet: */
#ifdef HAVE_CONFIG_H
#include "config.h"
#endif

#include "gui/pref-page-general.h"

#pragma GCC diagnostic push
#pragma GCC diagnostic ignored "-Wcast-qual"
#include <wx/filepicker.h>
#include <wx/wx.h>
#pragma GCC diagnostic pop

#include "gui/app.h"

namespace flint {
namespace gui {

PrefPageGeneral::PrefPageGeneral()
	: wxStockPreferencesPage(wxStockPreferencesPage::Kind_General)
{}

wxWindow *PrefPageGeneral::CreateWindow(wxWindow *parent)
{
	auto panel = new wxPanel(parent);
	auto box = new wxStaticBoxSizer(wxHORIZONTAL, panel, "Plotter");
	auto ctrl = new wxFilePickerCtrl(box->GetStaticBox(),
									 wxID_ANY,
									 wxEmptyString,
									 "gnuplot");
	auto text = new wxStaticText(box->GetStaticBox(),
								 wxID_ANY,
								 "gnuplot's executable:");
	box->Add(text, 0 /* horizontally unstretchable */, wxALIGN_CENTER_VERTICAL);
	box->Add(ctrl, 1 /* horizontally stretchable */, wxALIGN_CENTER_VERTICAL);
	panel->SetSizerAndFit(box);

	// events
	ctrl->Bind(wxEVT_FILEPICKER_CHANGED, &App::OnGnuplotExecutable, &wxGetApp());

	return panel;
}

}
}
