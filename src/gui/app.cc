/* -*- Mode: C++; tab-width: 4; indent-tabs-mode: t; c-basic-offset: 4 -*- vim:set ts=4 sw=4 sts=4 noet: */
#ifdef HAVE_CONFIG_H
#include "config.h"
#endif

#include "gui/app.h"

#include <boost/process/search_path.hpp>

#pragma GCC diagnostic push
#pragma GCC diagnostic ignored "-Wcast-qual"
#include <wx/config.h>
#include <wx/filename.h>
#pragma GCC diagnostic pop

#include "gui/main-frame.h"
#include "gui/pref-page-general.h"
#include "utf8path.h"

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

	pref_editor_ = nullptr;

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

boost::filesystem::path App::GetGnuplotExecutable() const
{
	boost::filesystem::path p(gnuplot_executable_.ToStdString());
	if (p.empty()) {
		// search executable from PATH
		p = boost::process::search_path("gnuplot");
	}
	return p;
}

void App::OnGnuplotExecutable(wxFileDirPickerEvent &event)
{
	gnuplot_executable_ = event.GetPath();
}

void App::ShowPreferencesEditor(wxWindow *parent)
{
	if (!pref_editor_) {
		pref_editor_ = new wxPreferencesEditor;
		pref_editor_->AddPage(new PrefPageGeneral(gnuplot_executable_));
	}
	pref_editor_->Show(parent);
}

}
}

wxIMPLEMENT_APP(flint::gui::App);
