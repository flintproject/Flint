/* -*- Mode: C++; tab-width: 4; indent-tabs-mode: t; c-basic-offset: 4 -*- vim:set ts=4 sw=4 sts=4 noet: */
#ifndef FLINT_GUI_GNUPLOT_H_
#define FLINT_GUI_GNUPLOT_H_

#include <iostream>
#include <map>

#pragma GCC diagnostic push
#pragma GCC diagnostic ignored "-Wcast-qual"
#include <wx/wx.h>
#pragma GCC diagnostic pop

namespace flint {
namespace gui {

struct LineGraphOption {
	std::map<unsigned int, wxString> input_files;
	unsigned int num_variables;
	unsigned int skip;
	int x;
	std::map<unsigned int, wxString> y1;
	std::map<unsigned int, wxString> y2;
	bool legend;
	bool log_x;
	bool log_y1;
	bool log_y2;
};

struct DpsGraphOption {
	wxString dps_path;
	unsigned int num_variables;
	unsigned int skip;
	std::map<unsigned int, unsigned int> m;
};

bool PlotLineGraph(const LineGraphOption &option,
				   const DpsGraphOption *dgo,
				   std::ostream &os);

}
}

#endif
