/* -*- Mode: C++; tab-width: 4; indent-tabs-mode: t; c-basic-offset: 4 -*- vim:set ts=4 sw=4 sts=4 noet: */
#ifndef FLINT_GUI_DESKTOP_H_
#define FLINT_GUI_DESKTOP_H_

#include <memory>
#include <set>

#include <wx/wx.h>

#include "gui/document.h"

namespace flint {
namespace gui {

class Desktop
{
public:
	Desktop();

	void OpenFile();

private:
	int AddDocument(const wxString &path);

	int next_id_;
	std::set<std::unique_ptr<Document> > docs_;
};

}
}

#endif
