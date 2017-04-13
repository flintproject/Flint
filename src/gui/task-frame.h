/* -*- Mode: C++; tab-width: 4; indent-tabs-mode: t; c-basic-offset: 4 -*- vim:set ts=4 sw=4 sts=4 noet: */
#ifndef FLINT_GUI_TASK_FRAME_H_
#define FLINT_GUI_TASK_FRAME_H_

#pragma GCC diagnostic push
#pragma GCC diagnostic ignored "-Wcast-qual"
#include <wx/wx.h>
#pragma GCC diagnostic pop

namespace flint {
namespace gui {

class JobWindow;
struct Task;

class TaskFrame : public wxFrame {
public:
	TaskFrame(wxWindow *parent, const Task &task);

	void Start();

private:
	void OnClose(wxCommandEvent &event);

	const Task &task_;
	JobWindow *job_window_;
};

}
}

#endif
