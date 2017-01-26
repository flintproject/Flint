/* -*- Mode: C++; tab-width: 4; indent-tabs-mode: t; c-basic-offset: 4 -*- vim:set ts=4 sw=4 sts=4 noet: */
#ifndef FLINT_GUI_CONFIGURATION_H_
#define FLINT_GUI_CONFIGURATION_H_

#include <string>
#include <vector>

#include <wx/wx.h>

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
	wxString start;
	int start_unit; // index
	int granularity;
	// Output Variables
	wxString filter_pattern;
	wxString filter_value;
	int filter_column; // index

	// Get 7-digit KISAO ID for the method
	const char *GetKisaoId() const;

	// For OutputStartTime attribute of SED-ML's uniformTimeCourse
	double GetOutputStartTime(const Document *doc) const;

	// For OutputEndTime attribute of SED-ML's uniformTimeCourse
	double GetOutputEndTime(const Document *doc) const;

	// For NumberOfPoints attribute of SED-ML's uniformTimeCourse
	int GetNumberOfPoints(const Document *doc) const;

	void GetOutputVariables(const Document *doc, std::vector<std::string> *v) const;
};

}
}

#endif
