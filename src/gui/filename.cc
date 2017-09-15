/* -*- Mode: C++; tab-width: 4; indent-tabs-mode: t; c-basic-offset: 4 -*- vim:set ts=4 sw=4 sts=4 noet: */
#ifdef HAVE_CONFIG_H
#include "config.h"
#endif

#include "gui/filename.h"

namespace flint {
namespace gui {

std::string GetFnStrFromWxFileName(const wxFileName &fileName)
{
	wxString fullPath = fileName.GetFullPath();
	return fullPath.ToStdString(); // TODO: check locale-dependency
}

boost::filesystem::path GetPathFromWxFileName(const wxFileName &fileName)
{
	wxString fullPath = fileName.GetFullPath();
	return GetPathFromWxString(fullPath);
}

boost::filesystem::path GetPathFromWxString(const wxString &str)
{
	boost::filesystem::path path(str.fn_str()); // TODO: check locale-dependency
	return path;
}

}
}
