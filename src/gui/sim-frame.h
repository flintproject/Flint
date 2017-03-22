/* -*- Mode: C++; tab-width: 4; indent-tabs-mode: t; c-basic-offset: 4 -*- vim:set ts=4 sw=4 sts=4 noet: */
#ifndef FLINT_GUI_SIM_FRAME_H_
#define FLINT_GUI_SIM_FRAME_H_

#pragma GCC diagnostic push
#pragma GCC diagnostic ignored "-Wcast-qual"
#include <wx/wx.h>
#pragma GCC diagnostic pop

#include "cli.pb.h"
#include "flint/error.h"

namespace flint {
namespace gui {

class MainFrame;
struct Simulation;
class TaskGauge;

class SimFrame : public wxFrame, public wxThreadHelper {
public:
	explicit SimFrame(MainFrame *parent, Simulation *sim);

	bool Start();

	void OnThreadUpdate(wxThreadEvent &event);

protected:
	virtual wxThread::ExitCode Entry() override;

private:
	void OnClose(wxCommandEvent &event);

	Simulation *sim_;
	cli::ExecOption option_;
	std::vector<TaskGauge *> gauges_;
	bool result_;
	wxCriticalSection cs_;
	StderrCapture ec_;
};

}
}

#endif
