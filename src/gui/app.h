/* -*- Mode: C++; tab-width: 4; indent-tabs-mode: t; c-basic-offset: 4 -*- vim:set ts=4 sw=4 sts=4 noet: */
#ifndef FLINT_GUI_APP_H_
#define FLINT_GUI_APP_H_

#pragma GCC diagnostic push
#pragma GCC diagnostic ignored "-Wcast-qual"
#include <wx/wx.h>
#include <wx/aui/aui.h>
#pragma GCC diagnostic pop

namespace flint {
namespace gui {

class App : public wxApp
{
public:
	virtual bool OnInit() override;
	virtual int OnExit() override;
};

}
}

#endif
