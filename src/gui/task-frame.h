/* -*- Mode: C++; tab-width: 4; indent-tabs-mode: t; c-basic-offset: 4 -*- vim:set ts=4 sw=4 sts=4 noet: */
#ifndef FLINT_GUI_TASK_FRAME_H_
#define FLINT_GUI_TASK_FRAME_H_

#define BOOST_DATE_TIME_NO_LIB
#include <boost/interprocess/mapped_region.hpp>

#pragma GCC diagnostic push
#pragma GCC diagnostic ignored "-Wcast-qual"
#include <wx/dataview.h>
#include <wx/wx.h>
#pragma GCC diagnostic pop

namespace flint {
namespace gui {

struct Job;
class JobWindow;
struct Task;
class ViewFrame;

class TaskFrame : public wxFrame {
public:
	TaskFrame(wxWindow *parent, const Task &task);

	const Task &task() const {return task_;}

	bool LoadItems();
	int AddParameterSample(int argc, char **argv, char **names);
	void Start();
	void View();

private:
	void OnCancel(wxCommandEvent &event);
	void OnItemContextMenu(wxDataViewEvent &event);
	void OnSelectionChanged(wxDataViewEvent &event);
	void OnExport(wxCommandEvent &event);
	void OnView(wxCommandEvent &event);
	void OnTimer(wxTimerEvent &event);
	void OnClose(wxCloseEvent &event);

	wxString GetRss(int job_id);
	void Export(const Job &job);
	void ExportAll();
	void ShowErrorOnExporting(const wxString &message);

	const Task &task_;
	boost::interprocess::mapped_region mr_;
	boost::interprocess::mapped_region rss_mr_;
	wxDataViewListCtrl *data_view_;
	wxButton *export_;
	wxButton *view_;
	ViewFrame *view_frame_;
	wxTimer timer_;
};

}
}

#endif
