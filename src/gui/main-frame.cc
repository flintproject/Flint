/* -*- Mode: C++; tab-width: 4; indent-tabs-mode: t; c-basic-offset: 4 -*- vim:set ts=4 sw=4 sts=4 noet: */
#ifdef HAVE_CONFIG_H
#include "config.h"
#endif

#include "gui/main-frame.h"

#include <wx/wx.h>
#include <wx/aboutdlg.h>
#include <wx/bookctrl.h>
#include <wx/config.h>
#include <wx/dnd.h>
#include <wx/filename.h>

#include "gui/sub-frame.h"

namespace flint {
namespace gui {

class ModelFileDropTarget : public wxFileDropTarget
{
public:
	explicit ModelFileDropTarget(MainFrame *frame);

	virtual bool OnDropFiles(wxCoord x, wxCoord y, const wxArrayString &filenames) override;

private:
	MainFrame *frame_;
};

ModelFileDropTarget::ModelFileDropTarget(MainFrame *frame)
	: frame_(frame)
{}

bool ModelFileDropTarget::OnDropFiles(wxCoord, wxCoord, const wxArrayString &filenames)
{
	auto n = filenames.GetCount();
	for (size_t i=0;i<n;i++) {
		if (!frame_->OpenFile(filenames[i]))
			return false;
	}
	return true;
}

enum {
	kIdRun,
	kIdPause,
	kIdResume,
	kIdSendToFlintK3
};

MainFrame::MainFrame()
	: wxFrame(nullptr, wxID_ANY, wxTheApp->GetAppDisplayName())
	, manager_()
	, notebook_(nullptr)
	, history_()
{
	manager_.SetManagedWindow(this);

	// menus
	auto menuFile = new wxMenu;
	menuFile->Append(wxID_OPEN);
	menuFile->Append(wxID_CLOSE);
	menuFile->AppendSeparator();
	menuFile->Append(wxID_EXIT);

	history_.Load(*wxConfig::Get());
	history_.UseMenu(menuFile);
	history_.AddFilesToMenu(menuFile);

	auto menuEdit = new wxMenu;
	menuEdit->Append(wxID_COPY);
	menuEdit->Append(wxID_CUT);
	menuEdit->AppendSeparator();
	menuEdit->Append(wxID_PREFERENCES);

	auto menuHelp = new wxMenu;
	menuHelp->Append(wxID_ABOUT);

	auto menuControl = new wxMenu;
	menuControl->Append(kIdRun, "&Run\tALT+R");
	menuControl->Append(kIdPause, "&Pause\tALT+P");
	menuControl->Append(kIdResume, "Re&sume\tALT+S");
	menuControl->Append(kIdSendToFlintK3, "Send to Flint K3");

	auto menuBar = new wxMenuBar;
	menuBar->Append(menuFile, wxGetStockLabel(wxID_FILE));
	menuBar->Append(menuEdit, wxGetStockLabel(wxID_EDIT));
	menuBar->Append(menuControl, "&Control");
	menuBar->Append(menuHelp, wxGetStockLabel(wxID_HELP));

	SetMenuBar(menuBar);

	CreateStatusBar();
	SetStatusText("Ready");

	SetMinSize(wxSize(600, 400));

	// panes
	auto buttonRun = new wxButton(this, wxID_ANY, "&Run");
	manager_.AddPane(buttonRun,
					 wxAuiPaneInfo().Name("simulation").Caption("Simulation").Bottom().Layer(1).Position(1));
	notebook_ = new wxAuiNotebook(this, wxID_ANY);
	notebook_->SetDropTarget(new ModelFileDropTarget(this));
	manager_.AddPane(notebook_,
					 wxAuiPaneInfo().Name("notebook").Caption("Notebook").CenterPane().PaneBorder(false));

	manager_.GetPane("notebook").Show();
	manager_.GetPane("simulation").Show();
	manager_.Update();

	// event handlers
	Bind(wxEVT_COMMAND_MENU_SELECTED, &MainFrame::OnOpen, this, wxID_OPEN);
	Bind(wxEVT_COMMAND_MENU_SELECTED, &MainFrame::OnClose, this, wxID_CLOSE);
	Bind(wxEVT_COMMAND_MENU_SELECTED, &MainFrame::OnAbout, this, wxID_ABOUT);
	Bind(wxEVT_COMMAND_MENU_SELECTED, &MainFrame::OnExit, this, wxID_EXIT);
	Bind(wxEVT_COMMAND_MENU_SELECTED, &MainFrame::OnRecentFile, this, wxID_FILE1, wxID_FILE9);
}

MainFrame::~MainFrame()
{
	manager_.UnInit();
}

bool MainFrame::OpenFile(const wxString &path)
{
	auto page = new wxNotebook(notebook_, wxID_ANY);
	page->AddPage(new GeneralSetttingsWindow(page), "General Settings", true);
	page->AddPage(new OutputVariablesWindow(page), "Output Variables");
	page->AddPage(new ParametersWindow(page), "Parameters");
	notebook_->AddPage(page, wxFileName(path).GetName(), true, 0);

	history_.AddFileToHistory(path);

	wxString text("Opened ");
	text += path;
	SetStatusText(text);
	return true;
}

void MainFrame::OnOpen(wxCommandEvent &)
{
	wxFileDialog openFileDialog(this,
								"Model file",
								"",
								"",
								"PHML files (*.phml)|*.phml",
								wxFD_OPEN|wxFD_FILE_MUST_EXIST);
	if (openFileDialog.ShowModal() == wxID_CANCEL)
		return;
	OpenFile(openFileDialog.GetPath());
}

void MainFrame::OnRecentFile(wxCommandEvent &event)
{
    auto path = history_.GetHistoryFile(event.GetId() - wxID_FILE1);
	if (path.empty())
		return;
	OpenFile(path);
}

void MainFrame::OnClose(wxCommandEvent &)
{
	auto i = notebook_->GetSelection();
	if (i == wxNOT_FOUND)
		return;
	wxString text("Closed ");
	text += notebook_->GetPageText(i);
	notebook_->DeletePage(i);
	SetStatusText(text);
}

void MainFrame::OnAbout(wxCommandEvent &)
{
	wxAboutDialogInfo aboutInfo;
	aboutInfo.SetName("Flint");
	aboutInfo.SetVersion("1.8");
	aboutInfo.SetDescription("A simulator for biological and physiological models");
	aboutInfo.SetCopyright("(C) 2015-2017 Okinawa Institute of Science and Technology Graduate University");
	aboutInfo.SetWebSite("https://flintproject.github.io/");
	wxAboutBox(aboutInfo);
}

void MainFrame::OnExit(wxCommandEvent &)
{
	history_.Save(*wxConfig::Get());
	Close(true);
}

}
}
