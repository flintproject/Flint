/* -*- Mode: C++; tab-width: 4; indent-tabs-mode: t; c-basic-offset: 4 -*- vim:set ts=4 sw=4 sts=4 noet: */
#ifdef HAVE_CONFIG_H
#include "config.h"
#endif

#include "gui/app.h"

#include <wx/config.h>
#include <wx/filename.h>

#include "gui/main-frame.h"

namespace flint {
namespace gui {

bool App::OnInit()
{
	if (!wxApp::OnInit())
		return false;

	SetAppDisplayName("Flint");
	SetVendorDisplayName("Flint project");

	wxFileName fileName;
	fileName.AssignHomeDir();
	fileName.AppendDir(".flint");
	fileName.Mkdir(wxS_DIR_DEFAULT, wxPATH_MKDIR_FULL); // make sure that it exists
	fileName.SetCwd();

	auto frame = new MainFrame;
	frame->CentreOnScreen();
	frame->Show();

	return true;
}

int App::OnExit()
{
	// clean up working directory
	wxFileName fileName;
	fileName.AssignHomeDir();
	fileName.SetCwd(); // change directory at first
	fileName.AppendDir(".flint");
	fileName.Rmdir(wxPATH_RMDIR_RECURSIVE);

	return wxApp::OnExit();
}

}
}

wxIMPLEMENT_APP(flint::gui::App);
