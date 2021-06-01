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
#include "cli.pb.h"
#include "filter/writer.h"
#include "flint/bc.h"
#include "flint/error.h"
#include "flint/ls.h"
#include "flint/tr.h"
#include "gui/app.h"
#include "gui/document.h"
#include "gui/filename.h"
#include "gui/phsp.h"
#include "gui/sedml.h"
#include "gui/sim-window.h"
#include "gui/simulation.h"
#include "gui/sub-window.h"
#include "lo/layout.h"
#include "load.h"
#include "../task.h" // TODO: fix collision between 2 task.h

#include <memory>
#include <string>
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
	kIdSaveConfigAs,
	kIdExportToC,
	kIdRun,
	kIdPause,
	kIdResume
};

}

MainFrame::MainFrame(wxArrayString &input_files)
	: wxFrame(nullptr, wxID_ANY, wxTheApp->GetAppDisplayName())
	, input_files_(input_files)
	, manager_(this)
	, next_open_id_(1)
	, next_simulation_id_(1)
	, last_dir_(wxFileName::GetHomeDir())
{
	// menus
	auto menuFile = new wxMenu;
	menuFile->Append(wxID_OPEN, "Open\tCTRL+O");
	menuFile->Append(wxID_CLOSE, "Close\tCTRL+W");
	menuFile->AppendSeparator();
	item_save_config_ = menuFile->Append(wxID_SAVE, "Save configuration\tCTRL+S");
	item_save_config_->Enable(false);
	item_save_config_as_ = menuFile->Append(kIdSaveConfigAs, "Save configuration as...\tSHIFT+CTRL+S");
	item_save_config_as_->Enable(false);
	menuFile->AppendSeparator();
	item_export_to_c_ = menuFile->Append(kIdExportToC, "Export to C");
	item_export_to_c_->Enable(false);
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
	item_run_ = menuControl->Append(kIdRun, "&Run\tALT+R");
	item_pause_ = menuControl->Append(kIdPause, "&Pause\tALT+P");
	item_resume_ = menuControl->Append(kIdResume, "Re&sume\tALT+S");

	auto menuBar = new wxMenuBar;
	menuBar->Append(menuFile, wxGetStockLabel(wxID_FILE));
	menuBar->Append(menuEdit, wxGetStockLabel(wxID_EDIT));
	menuBar->Append(menuControl, "&Control");
	menuBar->Append(menuHelp, wxGetStockLabel(wxID_HELP));

	SetMenuBar(menuBar);

	CreateStatusBar();
	SetStatusText("Ready");

	auto size = wxSize(750, 500);
	SetMinSize(size);
	SetSize(size);

	// panes
	button_run_ = new wxButton(this, wxID_ANY, "&Run");
	button_run_->Bind(wxEVT_BUTTON, &MainFrame::OnRun, this);
	manager_.AddPane(button_run_,
					 wxAuiPaneInfo().Name("simulation").Caption("Simulation").Bottom().Layer(0).Position(1));
	notebook_ = new wxAuiNotebook(this, wxID_ANY);
	notebook_->SetDropTarget(new ModelFileDropTarget(this));
	manager_.AddPane(notebook_,
					 wxAuiPaneInfo().Name("notebook").Caption("Notebook").CenterPane().PaneBorder(false));

	manager_.GetPane("notebook").Show();
	manager_.GetPane("simulation").Show();
	manager_.Update();

	ResetControl();

	// event handlers
	Bind(wxEVT_CLOSE_WINDOW, &MainFrame::OnCloseWindow, this);
	Bind(wxEVT_COMMAND_MENU_SELECTED, &MainFrame::OnOpen, this, wxID_OPEN);
	Bind(wxEVT_COMMAND_MENU_SELECTED, &MainFrame::OnClose, this, wxID_CLOSE);
	Bind(wxEVT_COMMAND_MENU_SELECTED, &MainFrame::OnAbout, this, wxID_ABOUT);
	Bind(wxEVT_COMMAND_MENU_SELECTED, &MainFrame::OnExit, this, wxID_EXIT);
	Bind(wxEVT_COMMAND_MENU_SELECTED, &MainFrame::OnSaveConfig, this, wxID_SAVE);
	Bind(wxEVT_COMMAND_MENU_SELECTED, &MainFrame::OnSaveConfigAs, this, kIdSaveConfigAs);
	Bind(wxEVT_COMMAND_MENU_SELECTED, &MainFrame::OnExportToC, this, kIdExportToC);
	Bind(wxEVT_COMMAND_MENU_SELECTED, &MainFrame::OnRun, this, kIdRun);
	Bind(wxEVT_COMMAND_MENU_SELECTED, &MainFrame::OnPause, this, kIdPause);
	Bind(wxEVT_COMMAND_MENU_SELECTED, &MainFrame::OnResume, this, kIdResume);
	Bind(wxEVT_COMMAND_MENU_SELECTED, &MainFrame::OnRecentFile, this, wxID_FILE1, wxID_FILE9);
	Bind(wxEVT_COMMAND_MENU_SELECTED, &MainFrame::OnPreferences, this, wxID_PREFERENCES);
	Bind(wxEVT_IDLE, &MainFrame::OnIdle, this);
	notebook_->Bind(wxEVT_AUINOTEBOOK_PAGE_CLOSE, &MainFrame::OnNotebookPageClose, this);
	notebook_->Bind(wxEVT_AUINOTEBOOK_PAGE_CLOSED, &MainFrame::OnNotebookPageClosed, this);
}

MainFrame::~MainFrame()
{
	manager_.UnInit();
}

void MainFrame::MakePauseAvailable()
{
	item_run_->Enable(false);
	item_pause_->Enable(true);
	item_resume_->Enable(false);
	button_run_->Enable(false);
}

void MainFrame::MakeResumeAvailable()
{
	item_run_->Enable(false);
	item_pause_->Enable(false);
	item_resume_->Enable(true);
	button_run_->Enable(false);
}

void MainFrame::ResetControl()
{
	item_run_->Enable(true);
	item_pause_->Enable(false);
	item_resume_->Enable(false);
	button_run_->Enable(true);
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
		auto buf = std::to_string(id_);
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
		doc_ = new Document(dir, path_, data);
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

	item_save_config_->Enable(true);
	item_save_config_as_->Enable(true);
	item_export_to_c_->Enable(true);

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
								"PHML files (*.phml;*.phz;*.isml)|*.phml;*.phz;*.isml"
								"|SBML files (*.xml)|*.xml"
								"|CellML files (*.cellml)|*.cellml"
								"|All files (*.*)|*.*",
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

	if (notebook_->GetPageCount() == 0) {
		item_save_config_->Enable(false);
		item_save_config_as_->Enable(false);
		item_export_to_c_->Enable(false);
	}
}

void MainFrame::OnCloseWindow(wxCloseEvent &)
{
	history_.Save(*wxConfig::Get());
	Destroy();
}

void MainFrame::OnAbout(wxCommandEvent &)
{
	wxAboutDialogInfo aboutInfo;
	aboutInfo.SetName("Flint");
	aboutInfo.SetVersion("2.5");
	aboutInfo.SetDescription("A simulator for biological and physiological models");
	aboutInfo.SetCopyright("(C) 2015-2020 Takeshi Abe\n"
						   "(C) 2015-2017 Okinawa Institute of Science and Technology Graduate University");
	aboutInfo.SetWebSite("https://flintsimulator.org/");
	wxAboutBox(aboutInfo);
}

void MainFrame::OnExit(wxCommandEvent &)
{
	Close();
}

void MainFrame::OnSaveConfig(wxCommandEvent &)
{
	auto count = notebook_->GetPageCount();
	if (count == 0)
		return;

	if (config_dir_.IsEmpty()) {
		wxDirDialog saveDirDialog(this,
								  "Target directory",
								  "", // TODO: defaultPath
								  wxDD_DEFAULT_STYLE|wxDD_DIR_MUST_EXIST);
		if (saveDirDialog.ShowModal() == wxID_CANCEL)
			return;
		config_dir_ = saveDirDialog.GetPath();
	}

	std::unique_ptr<Simulation> sim(new Simulation); // no need for id
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

	wxFileName filename(config_dir_, "input.xml");
	if (!WriteSedml(sim.get(), filename)) {
		wxLogError("failed to save into a SED-ML file");
		return;
	}
	filename.SetFullName("input.phsp");
	if (!WritePhsp(sim.get(), filename)) {
		wxLogError("failed to save into a PHSP file");
		return;
	}

	SetStatusText(wxString::Format("Saved configuration into %s", config_dir_));
}

void MainFrame::OnSaveConfigAs(wxCommandEvent &event)
{
	config_dir_.Clear();
	OnSaveConfig(event);
}

namespace {

class ExportToCHelper : public wxProgressDialog, public wxThreadHelper {
public:
	ExportToCHelper(const wxString &input_path,
					const wxString &output_path,
					MainFrame *frame);

	bool Start();

	void OnThreadUpdate(wxThreadEvent &event);

protected:
	virtual wxThread::ExitCode Entry() override;

private:
	const wxString input_path_;
	const wxString output_path_;
	MainFrame *frame_;
	bool result_;
	StderrCapture ec_;
};

ExportToCHelper::ExportToCHelper(const wxString &input_path,
								 const wxString &output_path,
								 MainFrame *frame)
	: wxProgressDialog("Exporting model", input_path, 100, frame, wxPD_APP_MODAL|wxPD_AUTO_HIDE)
	, input_path_(input_path)
	, output_path_(output_path)
	, frame_(frame)
	, result_(false)
{
	Bind(wxEVT_THREAD, &ExportToCHelper::OnThreadUpdate, this);
}

bool ExportToCHelper::Start()
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

void ExportToCHelper::OnThreadUpdate(wxThreadEvent &)
{
	if (result_) {
		wxMessageDialog dialog(frame_, "Exported successfully to " + output_path_);
		dialog.ShowModal();
	} else {
		wxMessageDialog dialog(frame_, ec_.Get(), "Failed to export model");
		dialog.ShowModal();
	}
	Destroy();
}

wxThread::ExitCode ExportToCHelper::Entry()
{
	cli::RunOption option;
	option.set_model_filename(input_path_.utf8_str().data());
	option.set_output_filename(output_path_.utf8_str().data());

	wxFileName fileName;
	fileName.AssignHomeDir();
	fileName.AppendDir(".flint");
	fileName.AppendDir("2");
	AppendCurrentTimestampDir(fileName);
	fileName.Mkdir(wxS_DIR_DEFAULT, wxPATH_MKDIR_FULL); // make sure that it exists

	boost::filesystem::path dir(fileName.GetFullPath().ToStdString());

	result_ = tr::Translate(option, dir);
	wxQueueEvent(this, new wxThreadEvent);
	return static_cast<wxThread::ExitCode>(0);
}

}

void MainFrame::OnExportToC(wxCommandEvent &)
{
	auto i = notebook_->GetSelection();
	if (i == wxNOT_FOUND)
		return;

	wxFileDialog saveFileDialog(this,
								"Select target C file",
								last_dir_,
								"",
								"C files (*.c)|*.c",
								wxFD_SAVE|wxFD_OVERWRITE_PROMPT);
	if (saveFileDialog.ShowModal() == wxID_CANCEL)
		return;

	auto page = notebook_->GetPage(i);
	auto notebook = wxStaticCast(page, wxNotebook);
	auto p0 = notebook->GetPage(0);
	auto gsw = wxStaticCast(p0, GeneralSetttingsWindow);
	auto doc = gsw->doc();

	auto helper = new ExportToCHelper(doc->path(), saveFileDialog.GetPath(), this);
	helper->Start();
}

void MainFrame::OnNotebookPageClose(wxAuiNotebookEvent &)
{
	auto i = notebook_->GetSelection();
	if (i == wxNOT_FOUND)
		return;
	wxString text("Closed ");
	text += notebook_->GetPageText(i);
	SetStatusText(text);
}

void MainFrame::OnNotebookPageClosed(wxAuiNotebookEvent &)
{
	if (notebook_->GetPageCount() == 0) {
		item_save_config_->Enable(false);
		item_save_config_as_->Enable(false);
		item_export_to_c_->Enable(false);
	}
}

void MainFrame::OnRun(wxCommandEvent &)
{
	auto count = notebook_->GetPageCount();
	if (count == 0)
		return;

	// Check whether the model file has been changed or not after load
	for (size_t i=0;i<count;i++) {
		auto page = notebook_->GetPage(i);
		auto notebook = wxStaticCast(page, wxNotebook);
		auto p0 = notebook->GetPage(0);
		auto gsw = wxStaticCast(p0, GeneralSetttingsWindow);
		auto doc = gsw->doc();
		if (doc->IsModified()) {
			wxMessageDialog dialog(this,
								   "Model \"" + doc->path() + "\" looks modified recently.\nWould you like to proceed?\n(Yes to run simulation;\n No to stop and reload it;\n Cancel to give up and do nothing)",
								   "The model looks modified recently",
								   wxYES_NO|wxCANCEL|wxNO_DEFAULT);
			auto r = dialog.ShowModal();
			if (r == wxID_YES) {
				doc->UpdateMtime();
			} else if (r == wxID_NO) {
				auto path = doc->path();
				// close the model at first
				notebook_->DeletePage(i);
				if (notebook_->GetPageCount() == 0) {
					item_save_config_->Enable(false);
					item_save_config_as_->Enable(false);
					item_export_to_c_->Enable(false);
				}
				// queue reload
				input_files_.Add(path);
				return;
			} else {
				return;
			}
		}
	}

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
	auto window = new SimWindow(this, sim);
	auto info = wxAuiPaneInfo()
		.Name(wxString::Format("job%d", sim->id))
		.Caption(wxString::Format("Job %d", sim->id))
		.Right()
		.Layer(0)
		.Position(0) // stack on top
		.DestroyOnClose(true);
	manager_.AddPane(window, info);
	manager_.Update();
	if (!window->Start(wxGetApp().GetConcurrency()))
		return; // Something wrong happens, but stay availabe

	arg_.paused = false;
	MakePauseAvailable();
	SetStatusText(wxString::Format("Job %d started", sim->id));
}

void MainFrame::OnPause(wxCommandEvent &)
{
	arg_.paused = true;
	MakeResumeAvailable();
}

void MainFrame::OnResume(wxCommandEvent &)
{
	{
		std::unique_lock<std::mutex> lock(arg_.mutex);
		arg_.paused = false;
	}
	arg_.cv.notify_all();
	MakePauseAvailable();
}

void MainFrame::OnPreferences(wxCommandEvent &)
{
	wxGetApp().ShowPreferencesEditor(this);
}

void MainFrame::OnIdle(wxIdleEvent &)
{
	for (const auto &file : input_files_)
		OpenFile(file);
	input_files_.Empty();
}

}
}
