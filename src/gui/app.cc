/* -*- Mode: C++; tab-width: 4; indent-tabs-mode: t; c-basic-offset: 4 -*- vim:set ts=4 sw=4 sts=4 noet: */
#ifdef HAVE_CONFIG_H
#include "config.h"
#endif

#include "gui/app.h"

#include <wx/config.h>

#include "gui/main-frame.h"

namespace flint {
namespace gui {

App::App()
	: wxApp()
{
}

bool App::OnInit()
{
	if (!wxApp::OnInit())
		return false;

	SetAppDisplayName("Flint");
	SetVendorDisplayName("Flint project");

	auto frame = new MainFrame;
	frame->CentreOnScreen();
	frame->Show();

	return true;
}

}
}

wxIMPLEMENT_APP(flint::gui::App);
