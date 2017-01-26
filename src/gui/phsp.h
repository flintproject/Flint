/* -*- Mode: C++; tab-width: 4; indent-tabs-mode: t; c-basic-offset: 4 -*- vim:set ts=4 sw=4 sts=4 noet: */
#ifndef FLINT_GUI_PHSP_H_
#define FLINT_GUI_PHSP_H_

#include <wx/filename.h>

#include "gui/simulation.h"

namespace flint {
namespace gui {

bool WritePhsp(const Simulation *sim, const wxFileName &filename);

}
}

#endif
