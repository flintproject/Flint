/* -*- Mode: C++; tab-width: 4; indent-tabs-mode: t; c-basic-offset: 4 -*- vim:set ts=4 sw=4 sts=4 noet: */
#ifdef HAVE_CONFIG_H
#include "config.h"
#endif

#include "gui/main-frame.h"

#define BOOST_FILESYSTEM_NO_DEPRECATED
#include <boost/filesystem.hpp>

#pragma GCC diagnostic push
#pragma GCC diagnostic ignored "-Wcast-qual"
#include <wx/aboutdlg.h>
#include <wx/bookctrl.h>
#include <wx/config.h>
#include <wx/dnd.h>
#include <wx/filename.h>
#include <wx/progdlg.h>
#pragma GCC diagnostic pop

#include "cas.h"
#include "filter/writer.h"
#include "flint/bc.h"
#include "flint/error.h"
#include "flint/ls.h"
#include "gui/app.h"
#include "gui/document.h"
#include "gui/sim-frame.h"
#include "gui/simulation.h"
#include "gui/sub-window.h"
#include "lo/layout.h"
#include "load.h"
#include "../task.h" // TODO: fix collision between 2 task.h

#include <memory>
#include <vector>

namespace flint {
namespace gui {

namespace {

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

}

MainFrame::MainFrame(wxArrayString &input_files)
	: wxFrame(nullptr, wxID_ANY, wxTheApp->GetAppDisplayName())
	, input_files_(input_files)
	, notebook_(nullptr)
	, next_open_id_(1)
	, next_simulation_id_(1)
	, last_dir_(wxFileName::GetHomeDir())
{
	manager_.SetManagedWindow(this);

	// menus
	auto menuFile = new wxMenu;
	menuFile->Append(wxID_OPEN, "Open\tCTRL+O");
	menuFile->Append(wxID_CLOSE, "Close\tCTRL+W");
	menuFile->AppendSeparator();
	menuFile->Append(wxID_EXIT, "Quit\tCTRL+Q");

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

	SetMinSize(wxSize(960, 600));

	// panes
	auto buttonRun = new wxButton(this, wxID_ANY, "&Run");
	buttonRun->Bind(wxEVT_BUTTON, &MainFrame::OnRun, this);
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
	Bind(wxEVT_COMMAND_MENU_SELECTED, &MainFrame::OnRun, this, kIdRun);
	Bind(wxEVT_COMMAND_MENU_SELECTED, &MainFrame::OnRecentFile, this, wxID_FILE1, wxID_FILE9);
	Bind(wxEVT_COMMAND_MENU_SELECTED, &MainFrame::OnPreferences, this, wxID_PREFERENCES);
	Bind(wxEVT_IDLE, &MainFrame::OnIdle, this);
}

MainFrame::~MainFrame()
{
	manager_.UnInit();
}

namespace {

class OpenFileHelper : public wxProgressDialog, public wxThreadHelper {
public:
	OpenFileHelper(int id, const wxString &path, MainFrame *frame);

	bool Start();

	void OnThreadUpdate(wxThreadEvent &event);

protected:
	virtual wxThread::ExitCode Entry() override;

private:
	int id_;
	const wxString path_;
	MainFrame *frame_;
	wxCriticalSection cs_;
	Document *doc_;
	StderrCapture ec_;
};

OpenFileHelper::OpenFileHelper(int id, const wxString &path, MainFrame *frame)
	: wxProgressDialog("Loading file", path, 100, frame, wxPD_APP_MODAL|wxPD_AUTO_HIDE)
	, id_(id)
	, path_(path)
	, frame_(frame)
	, doc_(nullptr)
{
	Bind(wxEVT_THREAD, &OpenFileHelper::OnThreadUpdate, this);
}

bool OpenFileHelper::Start()
{
	if (CreateThread(wxTHREAD_DETACHED) != wxTHREAD_NO_ERROR) {
		wxLogError("failed to create the helper thread");
		return false;
	}
	if (GetThread()->Run() != wxTHREAD_NO_ERROR) {
		wxLogError("failed to run the helper thread");
		return false;
	}
	return true;
}

void OpenFileHelper::OnThreadUpdate(wxThreadEvent &)
{
	if (doc_) {
		frame_->OpenSubFrame(doc_);
	} else {
		std::unique_ptr<wxMessageDialog> dialog(new wxMessageDialog(frame_, ec_.Get(), "Failed to open file"));
		dialog->ShowModal();
	}
	Destroy();
}

wxThread::ExitCode OpenFileHelper::Entry()
{
	auto utf8path = path_.utf8_str();

	auto dir = boost::filesystem::current_path();
	{
		char buf[64];
		std::sprintf(buf, "%d", id_);
		dir /= buf;
		boost::system::error_code ec;
		boost::filesystem::create_directory(dir, ec);
		if (ec) {
			wxQueueEvent(this, new wxThreadEvent);
			return static_cast<wxThread::ExitCode>(0);
		}
	}

	std::vector<double> data;
	std::unique_ptr<task::Task> task(load::Load(utf8path.data(), load::ConfigMode::kOpen, dir, &data));
	if (task) {
		wxCriticalSectionLocker lock(cs_);
		doc_ = new Document(id_, path_, data);
		if (!doc_->Load()) {
			delete doc_;
			doc_ = nullptr;
		}
	}
	wxQueueEvent(this, new wxThreadEvent);
	return static_cast<wxThread::ExitCode>(0);
}

}

bool MainFrame::OpenFile(const wxString &path)
{
	auto path_s = path.ToStdString();
	boost::filesystem::path p(path_s);
	auto pp = p.parent_path();
	last_dir_ = wxString(pp.string());

	auto helper = new OpenFileHelper(next_open_id_++, path, this);
	return helper->Start();
}

void MainFrame::OpenSubFrame(Document *doc)
{
	auto page = new wxNotebook(notebook_, wxID_ANY);
	page->AddPage(new GeneralSetttingsWindow(page, doc), "General Settings", true);
	page->AddPage(new OutputVariablesWindow(page, doc), "Output Variables");
	page->AddPage(new ParametersWindow(page, doc), "Parameters");
	page->AddPage(new ObjectiveWindow(page), "Objective");
	notebook_->AddPage(page, wxFileName(doc->path()).GetName(), true, 0);

	history_.AddFileToHistory(doc->path());

	wxString text("Opened ");
	text += doc->path();
	SetStatusText(text);
}

void MainFrame::OnOpen(wxCommandEvent &)
{
	wxFileDialog openFileDialog(this,
								"Model file",
								last_dir_,
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
	aboutInfo.SetVersion("1.9");
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

void MainFrame::OnRun(wxCommandEvent &)
{
	auto count = notebook_->GetPageCount();
	if (count == 0)
		return;
	auto sim = new Simulation;
	sim->id = next_simulation_id_++;
	for (size_t i=0;i<count;i++) {
		auto page = notebook_->GetPage(i);
		auto notebook = wxStaticCast(page, wxNotebook);
		auto p0 = notebook->GetPage(0);
		auto gsw = wxStaticCast(p0, GeneralSetttingsWindow);
		auto doc = gsw->doc();
		auto p1 = notebook->GetPage(1);
		auto ovw = wxStaticCast(p1, OutputVariablesWindow);
		auto p2 = notebook->GetPage(2);
		auto pw = wxStaticCast(p2, ParametersWindow);
		auto p3 = notebook->GetPage(3);
		auto ow = wxStaticCast(p3, ObjectiveWindow);
		std::unique_ptr<Configuration> config(new Configuration(doc->initial_config()));
		gsw->Write(config.get());
		ovw->Write(config.get());
		pw->Write(config.get());
		ow->Write(config.get());
		sim->entries.emplace_back(doc, config.release());
	}
	auto frame = new SimFrame(this, sim);
	frame->Start();
}

void MainFrame::OnPreferences(wxCommandEvent &)
{
	wxGetApp().ShowPreferencesEditor(this);
}

void MainFrame::OnIdle(wxIdleEvent &)
{
	SetStatusText("");

	for (const auto &file : input_files_)
		OpenFile(file);
	input_files_.Empty();
}

}
}
