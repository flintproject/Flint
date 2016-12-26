/* -*- Mode: C++; tab-width: 4; indent-tabs-mode: t; c-basic-offset: 4 -*- vim:set ts=4 sw=4 sts=4 noet: */
#ifndef FLINT_GUI_SUB_FRAME_H_
#define FLINT_GUI_SUB_FRAME_H_

#include <wx/wx.h>

namespace flint {
namespace gui {

class GeneralSetttingsWindow : public wxWindow
{
public:
	explicit GeneralSetttingsWindow(wxWindow *);
};

class OutputVariablesWindow : public wxWindow
{
public:
	explicit OutputVariablesWindow(wxWindow *);
};

class ParametersWindow : public wxWindow
{
public:
	explicit ParametersWindow(wxWindow *);
};

}
}

#endif
