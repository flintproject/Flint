/* -*- Mode: C++; tab-width: 4; indent-tabs-mode: t; c-basic-offset: 4 -*- vim:set ts=4 sw=4 sts=4 noet: */
#ifndef FLINT_GUI_SIMULATION_H_
#define FLINT_GUI_SIMULATION_H_

#include <utility>
#include <vector>

#include <wx/filename.h>

namespace flint {
namespace gui {

struct Configuration;
class Document;

struct Simulation {
	int id;
	std::vector<std::pair<Document *, const Configuration *> > entries;

	wxFileName GetDirectoryName();
	wxFileName GetProgressFileName(int i);
};

}
}

#endif
