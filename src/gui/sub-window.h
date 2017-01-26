/* -*- Mode: C++; tab-width: 4; indent-tabs-mode: t; c-basic-offset: 4 -*- vim:set ts=4 sw=4 sts=4 noet: */
#ifndef FLINT_GUI_SUB_FRAME_H_
#define FLINT_GUI_SUB_FRAME_H_

#include <wx/wx.h>

#include "gui/document.h"

namespace flint {
namespace gui {

class GeneralSetttingsWindow : public wxWindow
{
public:
	GeneralSetttingsWindow(wxWindow *parent, Document *doc);

	Document *doc() {return doc_;}

private:
	Document *doc_;
};

class OutputVariablesWindow : public wxWindow
{
public:
	OutputVariablesWindow(wxWindow *parent, Document *doc);

private:
	Document *doc_;
};

class ParametersWindow : public wxWindow
{
public:
	ParametersWindow(wxWindow *parent, Document *doc);

private:
	Document *doc_;
};

}
}

#endif
