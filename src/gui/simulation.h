/* -*- Mode: C++; tab-width: 4; indent-tabs-mode: t; c-basic-offset: 4 -*- vim:set ts=4 sw=4 sts=4 noet: */
#ifndef FLINT_GUI_SIMULATION_H_
#define FLINT_GUI_SIMULATION_H_

#include <utility>
#include <vector>

#pragma GCC diagnostic push
#pragma GCC diagnostic ignored "-Wcast-qual"
#include <wx/filename.h>
#pragma GCC diagnostic pop

namespace flint {
namespace gui {

struct Configuration;
class Document;

struct Simulation {
	int id;
	std::vector<std::pair<Document *, const Configuration *> > entries;

	wxFileName GetDirectoryName() const;
	wxFileName GetProgressFileName(int i) const;
	wxFileName GetRssFileName(int i) const;
};

}
}

#endif
