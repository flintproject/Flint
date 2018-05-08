/* -*- Mode: C++; tab-width: 4; indent-tabs-mode: t; c-basic-offset: 4 -*- vim:set ts=4 sw=4 sts=4 noet: */
#ifndef FLINT_GUI_PREFERENCE_H_
#define FLINT_GUI_PREFERENCE_H_

#pragma GCC diagnostic push
#pragma GCC diagnostic ignored "-Wcast-qual"
#include <wx/wx.h>
#pragma GCC diagnostic pop

namespace flint {
namespace gui {

struct Preference
{
	unsigned int concurrency;
	wxString gnuplot_executable;

	Preference();
	~Preference();

	Preference(const Preference &) = delete;
	Preference &operator=(const Preference &) = delete;
};

}
}

#endif
