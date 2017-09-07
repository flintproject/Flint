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
#include <wx/persist.h>
#pragma GCC diagnostic pop

#include "gui/main-frame.h"
#include "gui/pref-page-general.h"
#include "utf8path.h"

namespace flint {
namespace gui {

namespace {

class PersistentApp : public wxPersistentObject
{
public:
	PersistentApp(App *, wxString &gnuplot_executable);

	virtual void Save() const override;
	virtual bool Restore() override;
	virtual wxString GetKind() const override;
	virtual wxString GetName() const override;

private:
	wxString &gnuplot_executable_;
};

PersistentApp::PersistentApp(App *obj, wxString &gnuplot_executable)
	: wxPersistentObject(obj)
	, gnuplot_executable_(gnuplot_executable)
{
}

void PersistentApp::Save() const
{
	SaveValue("gnuplot_executable", gnuplot_executable_);
}

bool PersistentApp::Restore()
{
	return RestoreValue("gnuplot_executable", &gnuplot_executable_);
}

wxString PersistentApp::GetKind() const
{
	return "Preference";
}

wxString PersistentApp::GetName() const
{
	return "File";
}

}

bool App::OnInit()
{
	if (!wxApp::OnInit())
		return false;

	checker_ = new wxSingleInstanceChecker;
	if (checker_->IsAnotherRunning()) {
		wxLogError("Another instance of this program is already running, aborting.");
		delete checker_;
		return false;
	}

	SetAppDisplayName("Flint");
	SetVendorDisplayName("Flint project");

	wxPersistenceManager::Get().RegisterAndRestore(this, new PersistentApp(this, gnuplot_executable_));

	wxFileName fileName;
	fileName.AssignHomeDir();
	fileName.AppendDir(".flint");
	fileName.AppendDir("2");
	fileName.Rmdir(wxPATH_RMDIR_RECURSIVE); // clean up working directory at first
	fileName.Mkdir(wxS_DIR_DEFAULT, wxPATH_MKDIR_FULL); // make sure that it exists
	fileName.SetCwd();

	auto frame = new MainFrame;
	frame->Show();

	pref_editor_ = nullptr;

	return true;
}

int App::OnExit()
{
	wxPersistenceManager::Get().SaveAndUnregister(this);

	delete checker_;

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
