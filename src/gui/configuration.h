/* -*- Mode: C++; tab-width: 4; indent-tabs-mode: t; c-basic-offset: 4 -*- vim:set ts=4 sw=4 sts=4 noet: */
#ifndef FLINT_GUI_CONFIGURATION_H_
#define FLINT_GUI_CONFIGURATION_H_

#include <string>
#include <utility>
#include <unordered_map>
#include <vector>

#pragma GCC diagnostic push
#pragma GCC diagnostic ignored "-Wcast-qual"
#include <wx/wx.h>
#pragma GCC diagnostic pop

#include "lo.pb.h"
#include "gui/param-tree.h"

namespace flint {
namespace gui {

class Document;

struct Configuration
{
	// General Settings
	wxString method;
	wxString length;
	int length_unit; // index
	wxString step;
	int step_unit; // index
	double start;
	int start_unit; // index
	int granularity;
	// Output Variables
	wxString filter_pattern;
	wxString filter_value;
	int filter_column; // index
	// Parameters
	std::unordered_map<int, std::string> parameters;
	ParamMap param_map;

	// Get 7-digit KISAO ID for the method
	const char *GetKisaoId() const;

	// For OutputStartTime attribute of SED-ML's uniformTimeCourse
	double GetOutputStartTime(const Document *doc) const;

	// For OutputEndTime attribute of SED-ML's uniformTimeCourse
	double GetOutputEndTime(const Document *doc) const;

	// For NumberOfPoints attribute of SED-ML's uniformTimeCourse
	int GetNumberOfPoints(const Document *doc) const;

	void GetOutputVariables(const Document *doc, std::vector<lo::Column> *v) const;
	void GetOutputVariables(const Document *doc, std::vector<std::string> *v) const;

	// the content of the string is in UTF-8.
	std::vector<std::pair<lo::Column, std::string> > GetTargets(const Document *doc) const;
};

}
}

#endif
