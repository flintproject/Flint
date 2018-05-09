/* -*- Mode: C++; tab-width: 4; indent-tabs-mode: t; c-basic-offset: 4 -*- vim:set ts=4 sw=4 sts=4 noet: */
#ifndef FLINT_GUI_APP_H_
#define FLINT_GUI_APP_H_

#include <memory>

#include <boost/filesystem.hpp>

#pragma GCC diagnostic push
#pragma GCC diagnostic ignored "-Wcast-qual"
#include <wx/wx.h>
#include <wx/cmdline.h>
#include <wx/filepicker.h>
#include <wx/preferences.h>
#include <wx/snglinst.h>
#include <wx/spinbutt.h>
#pragma GCC diagnostic pop

namespace flint {
namespace gui {

class Httpd;
class Preference;

class App : public wxApp
{
public:
	virtual bool OnInit() override;
	virtual void OnInitCmdLine(wxCmdLineParser &parser) override;
	virtual bool OnCmdLineParsed(wxCmdLineParser &parser) override;
	virtual int OnExit() override;

	boost::filesystem::path GetGnuplotExecutable() const;

	void OnConcurrency(wxSpinEvent &event);
	void OnGnuplotExecutable(wxFileDirPickerEvent &event);

	void ShowPreferencesEditor(wxWindow *parent);

private:
	wxSingleInstanceChecker *checker_;
	wxPreferencesEditor *pref_editor_;
	std::unique_ptr<Preference> preference_;
	wxArrayString input_files_;
	std::unique_ptr<Httpd> httpd_;
};

}
}

wxDECLARE_APP(flint::gui::App);

#endif
