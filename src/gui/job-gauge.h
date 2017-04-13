/* -*- Mode: C++; tab-width: 4; indent-tabs-mode: t; c-basic-offset: 4 -*- vim:set ts=4 sw=4 sts=4 noet: */
#ifndef FLINT_GUI_TASK_GAUGE_H_
#define FLINT_GUI_TASK_GAUGE_H_

#define BOOST_DATE_TIME_NO_LIB
#include <boost/interprocess/mapped_region.hpp>

#pragma GCC diagnostic push
#pragma GCC diagnostic ignored "-Wcast-qual"
#include <wx/wx.h>
#pragma GCC diagnostic pop

namespace flint {
namespace gui {

struct Job;

class JobGauge : public wxGauge, public wxThreadHelper
{
public:
	explicit JobGauge(wxWindow *parent, const Job &job);

	bool Start();

	void OnThreadUpdate(wxThreadEvent &event);

protected:
	virtual wxThread::ExitCode Entry() override;

private:
	boost::interprocess::mapped_region mr_;
};

}
}

#endif
