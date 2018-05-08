/* -*- Mode: C++; tab-width: 4; indent-tabs-mode: t; c-basic-offset: 4 -*- vim:set ts=4 sw=4 sts=4 noet: */
#ifdef HAVE_CONFIG_H
#include "config.h"
#endif

#include "gui/pref-page-general.h"

#pragma GCC diagnostic push
#pragma GCC diagnostic ignored "-Wcast-qual"
#include <wx/filepicker.h>
#include <wx/spinctrl.h>
#include <wx/wx.h>
#pragma GCC diagnostic pop

#include "gui/app.h"
#include "gui/preference.h"

namespace flint {
namespace gui {

PrefPageGeneral::PrefPageGeneral(Preference &preference)
	: wxStockPreferencesPage(wxStockPreferencesPage::Kind_General)
	, preference_(preference)
{}

wxWindow *PrefPageGeneral::CreateWindow(wxWindow *parent)
{
	auto panel = new wxPanel(parent);

	auto box_c = new wxStaticBoxSizer(wxHORIZONTAL, panel, "Concurrency hint");
	auto ctrl_c = new wxSpinCtrl(box_c->GetStaticBox(),
								 wxID_ANY,
								 wxString::Format("%i", preference_.concurrency),
								 wxDefaultPosition,
								 wxDefaultSize,
								 wxSP_ARROW_KEYS|wxALIGN_RIGHT,
								 1,
								 1000,
								 1);
	auto text_c = new wxStaticText(box_c->GetStaticBox(),
								   wxID_ANY,
								   "The number of concurrent threads:");
	box_c->Add(text_c, 0 /* horizontally unstretchable */, wxALIGN_CENTER_VERTICAL);
	box_c->Add(ctrl_c, 1 /* horizontally stretchable */, wxALIGN_CENTER_VERTICAL);

	auto box = new wxStaticBoxSizer(wxHORIZONTAL, panel, "Plotter");
	auto ctrl = new wxFilePickerCtrl(box->GetStaticBox(),
									 wxID_ANY,
									 preference_.gnuplot_executable,
									 "gnuplot");
	auto text = new wxStaticText(box->GetStaticBox(),
								 wxID_ANY,
								 "Gnuplot executable:");
	box->Add(text, 0 /* horizontally unstretchable */, wxALIGN_CENTER_VERTICAL);
	box->Add(ctrl, 1 /* horizontally stretchable */, wxALIGN_CENTER_VERTICAL);

	auto vbox = new wxBoxSizer(wxVERTICAL);
	vbox->Add(box_c, 0, wxEXPAND /* horizontally stretchable */);
	vbox->Add(box, 0, wxEXPAND /* horizontally stretchable */);
	panel->SetSizerAndFit(vbox);

	// events
	ctrl->Bind(wxEVT_FILEPICKER_CHANGED, &App::OnGnuplotExecutable, &wxGetApp());

	return panel;
}

}
}
