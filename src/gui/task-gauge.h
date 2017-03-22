/* -*- Mode: C++; tab-width: 4; indent-tabs-mode: t; c-basic-offset: 4 -*- vim:set ts=4 sw=4 sts=4 noet: */
#ifndef FLINT_GUI_TASK_GAUGE_H_
#define FLINT_GUI_TASK_GAUGE_H_

#pragma GCC diagnostic push
#pragma GCC diagnostic ignored "-Wcast-qual"
#include <wx/filename.h>
#include <wx/wx.h>
#pragma GCC diagnostic pop

namespace flint {
namespace gui {

class TaskGauge : public wxGauge, public wxThreadHelper
{
public:
	explicit TaskGauge(wxWindow *parent, wxFileName filename);

	bool Start();

	void OnThreadUpdate(wxThreadEvent &event);

protected:
	virtual wxThread::ExitCode Entry() override;

private:
	wxFileName filename_;
};

}
}

#endif
