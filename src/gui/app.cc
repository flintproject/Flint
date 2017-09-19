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

#include "cli.pb.h"
#include "gui/filename.h"
#include "gui/httpd.h"
#include "gui/main-frame.h"
#include "gui/pref-page-general.h"
#include "run.h"
#include "utf8path.h"

#include <cstdlib>
#include <cstdio>
#include <iostream>
#include <limits>

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

const wxCmdLineEntryDesc kCommandLineDesc[] =
{
    { wxCMD_LINE_SWITCH, "h", "help", "show this help message", wxCMD_LINE_VAL_NONE, wxCMD_LINE_OPTION_HELP },
    { wxCMD_LINE_SWITCH, "headless", "headless", "enable the headless mode", wxCMD_LINE_VAL_NONE, 0 },
    { wxCMD_LINE_OPTION, "e", nullptr,
	  "save error messages as specified file (only with the headless mode)",
	  wxCMD_LINE_VAL_STRING, 0 },
    { wxCMD_LINE_OPTION, "g", nullptr,
	  "specify output sampling rate i.e. 1 output per given step (only with the headless mode)",
	  wxCMD_LINE_VAL_NUMBER, 0 },
    { wxCMD_LINE_OPTION, "s", nullptr,
	  "choose output variables with specified file (only with the headless mode)",
	  wxCMD_LINE_VAL_STRING, 0 },
    { wxCMD_LINE_PARAM, nullptr, nullptr, "file", wxCMD_LINE_VAL_STRING, wxCMD_LINE_PARAM_OPTIONAL|wxCMD_LINE_PARAM_MULTIPLE },
    { wxCMD_LINE_NONE, nullptr, nullptr, nullptr, wxCMD_LINE_VAL_NONE, 0 } // the same as { wxCMD_LINE_NONE }, and suppress compiler warnig
};

/*
 * We need this as we have to change directory before simulation.
 */
wxFileName GetAbsoluteFilename(const wxString &file)
{
	wxFileName fileName(file);
	if (!fileName.IsAbsolute())
		fileName.MakeAbsolute();
	return fileName;
}

std::string GetAbsoluteFilenameInUtf8(const wxString &file)
{
	auto fileName = GetAbsoluteFilename(file);
	return fileName.GetFullPath().utf8_str().data();
}

wxFileName GetFlintDirectory()
{
	wxFileName fileName;
	fileName.AssignHomeDir();
	fileName.AppendDir(".flint");
	fileName.AppendDir("2");
	return fileName;
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

	auto fileName = GetFlintDirectory();
	fileName.Rmdir(wxPATH_RMDIR_RECURSIVE); // clean up working directory at first
	fileName.Mkdir(wxS_DIR_DEFAULT, wxPATH_MKDIR_FULL); // make sure that it exists
	fileName.SetCwd();

	auto frame = new MainFrame(input_files_);
	SetTopWindow(frame);
	frame->CentreOnScreen();
	frame->Show();

	pref_editor_ = nullptr;

	httpd_.reset(new Httpd);
	httpd_->Start(frame);

	return true;
}

void App::OnInitCmdLine(wxCmdLineParser &parser)
{
	parser.SetDesc(kCommandLineDesc);
	parser.SetSwitchChars("-");
}

bool App::OnCmdLineParsed(wxCmdLineParser &parser)
{
	if (parser.Found("headless")) {
		// the headless mode
		if (parser.GetParamCount() != 2) {
			std::cerr << "just 2 arguments (i.e. <input> and <output>) needed for the headless mode" << std::endl;
			std::exit(EXIT_FAILURE);
		}
		wxString param0 = parser.GetParam(0);
		wxString param1 = parser.GetParam(1);
		cli::RunOption option;
		option.set_model_filename(GetAbsoluteFilenameInUtf8(param0));
		option.set_output_filename(GetAbsoluteFilenameInUtf8(param1));
		wxString e;
		if (parser.Found("e", &e))
			option.set_error_filename(GetAbsoluteFilenameInUtf8(e));
		long g;
		if (parser.Found("g", &g)) {
			if (g <= 0) {
				std::cerr << "non-positive value is invalid for -g option: "
						  << g
						  << std::endl;
				std::exit(EXIT_FAILURE);
			}
			if (std::numeric_limits<int>::max() < g) {
				std::cerr << "too huge value for -g option: "
						  << g
						  << std::endl;
				std::exit(EXIT_FAILURE);
			}
			option.set_granularity(static_cast<int>(g));
		}
		wxString s;
		if (parser.Found("s", &s))
			option.set_spec_filename(GetAbsoluteFilenameInUtf8(s));

		auto now = wxDateTime::Now();
		auto fileName = GetFlintDirectory();
		fileName.AppendDir(now.Format("%F %T"));
		fileName.Mkdir(wxS_DIR_DEFAULT, wxPATH_MKDIR_FULL); // make sure that it exists
		std::exit(run::Run(option, GetPathFromWxFileName(fileName)) ? EXIT_SUCCESS : EXIT_FAILURE);
	}

	for (const auto &arg : parser.GetArguments()) {
		if (arg.GetKind() == wxCMD_LINE_PARAM) {
			auto fileName = GetAbsoluteFilename(arg.GetStrVal());
			input_files_.Add(fileName.GetFullPath());
		}
	}
	return true;
}

int App::OnExit()
{
	httpd_.reset();

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
