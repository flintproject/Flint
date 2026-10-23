/* -*- Mode: C++; tab-width: 4; indent-tabs-mode: t; c-basic-offset: 4 -*- vim:set ts=4 sw=4 sts=4 noet: */
#ifndef FLINT_GUI_ICON_H_
#define FLINT_GUI_ICON_H_

#include <wx/toplevel.h>

#ifdef __WXMSW__
#include <wx/bitmap.h>
#include <wx/icon.h>
#endif

namespace flint {
namespace gui {

// Give a top-level window the icon of the application. On Windows, the icon
// is loaded from the resources embedded in the executable by flint.rc; on the
// other platforms, it is provided by the bundle or the desktop entry.
inline void SetFlintIcon(wxTopLevelWindow *window) {
#ifdef __WXMSW__
	window->SetIcon(wxIcon("flint", wxBITMAP_TYPE_ICO_RESOURCE));
#else
	static_cast<void>(window);
#endif
}

}
}

#endif
