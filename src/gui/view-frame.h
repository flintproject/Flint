/* -*- Mode: C++; tab-width: 4; indent-tabs-mode: t; c-basic-offset: 4 -*- vim:set ts=4 sw=4 sts=4 noet: */
#ifndef FLINT_GUI_VIEW_FRAME_H_
#define FLINT_GUI_VIEW_FRAME_H_

#pragma GCC diagnostic push
#pragma GCC diagnostic ignored "-Wcast-qual"
#include <wx/dataview.h>
#include <wx/wx.h>
#pragma GCC diagnostic pop

namespace flint {
namespace gui {

class TaskFrame;

class ViewFrame : public wxFrame {
public:
	explicit ViewFrame(TaskFrame *parent);

private:
	void OnItemValueChanged(wxDataViewEvent &event);
	void OnClose(wxCloseEvent &event);

	bool LoadVariables();

	wxDataViewListCtrl *data_view_;
};

}
}

#endif
