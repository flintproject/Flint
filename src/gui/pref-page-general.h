/* -*- Mode: C++; tab-width: 4; indent-tabs-mode: t; c-basic-offset: 4 -*- vim:set ts=4 sw=4 sts=4 noet: */
#ifndef FLINT_GUI_PREF_PAGE_GENERAL_H_
#define FLINT_GUI_PREF_PAGE_GENERAL_H_

#pragma GCC diagnostic push
#pragma GCC diagnostic ignored "-Wcast-qual"
#include <wx/preferences.h>
#pragma GCC diagnostic pop

namespace flint {
namespace gui {

class Preference;

class PrefPageGeneral : public wxStockPreferencesPage {
public:
	explicit PrefPageGeneral(Preference &preference);

	virtual wxWindow *CreateWindow(wxWindow *parent) override;

private:
	Preference &preference_;
};

}
}

#endif
