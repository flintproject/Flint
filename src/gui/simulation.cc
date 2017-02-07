/* -*- Mode: C++; tab-width: 4; indent-tabs-mode: t; c-basic-offset: 4 -*- vim:set ts=4 sw=4 sts=4 noet: */
#ifdef HAVE_CONFIG_H
#include "config.h"
#endif

#include "gui/simulation.h"

#include "gui/configuration.h"
#include "gui/document.h"

namespace flint {
namespace gui {

wxFileName Simulation::GetDirectoryName()
{
	wxFileName filename;
	filename.AssignHomeDir();
	filename.AppendDir(".flint");
	filename.AppendDir("simulation");
	filename.AppendDir(wxString::Format("%d", id));
	return filename;
}

wxFileName Simulation::GetProgressFileName(int i)
{
	wxFileName filename = GetDirectoryName();
	filename.AppendDir(wxString::Format("%d", i));
	filename.SetFullName("progress");
	return filename;
}

}
}
