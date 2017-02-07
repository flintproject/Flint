/* -*- Mode: C++; tab-width: 4; indent-tabs-mode: t; c-basic-offset: 4 -*- vim:set ts=4 sw=4 sts=4 noet: */
#ifdef HAVE_CONFIG_H
#include "config.h"
#endif

#include "gui/desktop.h"

#include "gui/document.h"

namespace flint {
namespace gui {

Desktop::Desktop()
	: next_id_(0)
{
}

Desktop::~Desktop() = default;

int Desktop::AddDocument(const wxString &path)
{
	int id = ++next_id_;
	//docs_.emplace(new Document(id, path));
	return id;
}

}
}
