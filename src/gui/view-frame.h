/* -*- Mode: C++; tab-width: 4; indent-tabs-mode: t; c-basic-offset: 4 -*- vim:set ts=4 sw=4 sts=4 noet: */
#ifndef FLINT_GUI_VIEW_FRAME_H_
#define FLINT_GUI_VIEW_FRAME_H_

#include <cstdio>

#pragma GCC diagnostic push
#pragma GCC diagnostic ignored "-Wcast-qual"
#include <wx/dataview.h>
#include <wx/wx.h>
#pragma GCC diagnostic pop

namespace flint {
namespace gui {

struct Job;
class TaskFrame;

class ViewFrame : public wxFrame {
public:
	ViewFrame(TaskFrame *parent, wxDataViewListCtrl &job_list);

	void Plot();

private:
	void OnItemValueChanged(wxDataViewEvent &event);
	void OnClose(wxCloseEvent &event);

	bool LoadVariables();

	wxDataViewListCtrl &job_list_;
	wxDataViewListCtrl *data_view_;
	unsigned int num_variables_;
	unsigned int skip_;
	FILE *fp_;
};

}
}

#endif
