/* -*- Mode: C++; tab-width: 4; indent-tabs-mode: t; c-basic-offset: 4 -*- vim:set ts=4 sw=4 sts=4 noet: */
#ifndef FLINT_GUI_VIEW_FRAME_H_
#define FLINT_GUI_VIEW_FRAME_H_

#include <cstdio>
#include <memory>

#include <boost/process/child.hpp>
#include <boost/process/pipe.hpp>

#pragma GCC diagnostic push
#pragma GCC diagnostic ignored "-Wcast-qual"
#include <wx/dataview.h>
#include <wx/wx.h>
#pragma GCC diagnostic pop

namespace flint {
namespace gui {

struct DpsGraphOption;
struct Job;
class TaskFrame;

class ViewFrame : public wxFrame {
public:
	ViewFrame(TaskFrame *parent, wxDataViewListCtrl &job_list);

	void Plot();

private:
	void OnCheckBox(wxCommandEvent &event);
	void OnItemValueChanged(wxDataViewEvent &event);
	void OnClose(wxCloseEvent &event);

	bool LoadVariables();

	wxDataViewListCtrl &job_list_;
	wxDataViewListCtrl *data_view_;
	wxCheckBox *legend_;
	wxCheckBox *log_x_;
	wxCheckBox *log_y1_;
	wxCheckBox *log_y2_;
	unsigned int num_variables_;
	unsigned int skip_;
	std::unique_ptr<DpsGraphOption> dgo_;
	std::unique_ptr<boost::process::child> child_;
	boost::process::opstream pipe_;
};

}
}

#endif
