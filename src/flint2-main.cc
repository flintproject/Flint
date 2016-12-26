/* -*- Mode: C++; tab-width: 4; indent-tabs-mode: t; c-basic-offset: 4 -*- vim:set ts=4 sw=4 sts=4 noet: */
#ifdef HAVE_CONFIG_H
#include "config.h"
#endif

#include <wx/wxprec.h>
#ifndef WX_PRECOMP
#include <wx/wx.h>
#endif
#include <wx/config.h>

#include "gui/main-frame.h"

class FlintApp: public wxApp
{
public:
	FlintApp();

    virtual bool OnInit() override;
    virtual int OnExit() override;

private:
	wxDocManager *manager_;
};

FlintApp::FlintApp()
	: wxApp()
	, manager_(new wxDocManager)
{
}

bool FlintApp::OnInit()
{
	if (!wxApp::OnInit())
		return false;

	SetAppDisplayName("Flint");
	SetVendorDisplayName("Flint project");

	MainFrame *frame = new MainFrame(manager_);
	frame->Centre();
	frame->Show();
    return true;
}

int FlintApp::OnExit()
{
	manager_->FileHistorySave(*wxConfig::Get());
	delete manager_;
	return wxApp::OnExit();
}

wxIMPLEMENT_APP(FlintApp);
