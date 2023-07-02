/* -*- Mode: C++; tab-width: 4; indent-tabs-mode: t; c-basic-offset: 4 -*- vim:set ts=4 sw=4 sts=4 noet: */
#ifndef FLINT_GUI_TASK_WINDOW_H_
#define FLINT_GUI_TASK_WINDOW_H_

#pragma GCC diagnostic push
#pragma GCC diagnostic ignored "-Wcast-qual"
#include <wx/wx.h>
#pragma GCC diagnostic pop

#include "gui/task.h"

namespace flint {
namespace gui {

struct Simulation;
class TaskGauge;

class TaskWindow : public wxWindow {
public:
	TaskWindow(wxWindow *parent, Simulation *sim, int i);

	TaskGauge *gauge() {return gauge_;}

	void NotifyClose();

private:
	void OnDetail(wxCommandEvent &event);
	void OnX(wxCommandEvent &event);

	TaskGauge *gauge_;
	Task task_;
};

}
}

#endif
