/* -*- Mode: C++; tab-width: 4; indent-tabs-mode: t; c-basic-offset: 4 -*- vim:set ts=4 sw=4 sts=4 noet: */
#ifndef FLINT_GUI_SIM_WINDOW_H_
#define FLINT_GUI_SIM_WINDOW_H_

#pragma GCC diagnostic push
#pragma GCC diagnostic ignored "-Wcast-qual"
#include <wx/wx.h>
#include <wx/aui/framemanager.h>
#pragma GCC diagnostic pop

#include "cli.pb.h"
#include "flint/error.h"

namespace flint {
namespace gui {

class MainFrame;
struct Simulation;
class TaskWindow;

class SimWindow : public wxWindow, public wxThreadHelper {
public:
	explicit SimWindow(MainFrame *parent, Simulation *sim);

	~SimWindow();

	bool Start();

	void OnThreadUpdate(wxThreadEvent &event);

protected:
	virtual wxThread::ExitCode Entry() override;

private:
	Simulation *sim_;
	cli::ExecOption option_;
	std::vector<TaskWindow *> windows_;
	bool result_;
	StderrCapture ec_;
};

}
}

#endif
