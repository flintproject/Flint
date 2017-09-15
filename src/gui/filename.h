/* -*- Mode: C++; tab-width: 4; indent-tabs-mode: t; c-basic-offset: 4 -*- vim:set ts=4 sw=4 sts=4 noet: */
#ifndef FLINT_GUI_FILENAME_H_
#define FLINT_GUI_FILENAME_H_

#include <string>

#define BOOST_FILESYSTEM_NO_DEPRECATED
#include <boost/filesystem.hpp>

#pragma GCC diagnostic push
#pragma GCC diagnostic ignored "-Wcast-qual"
#include <wx/filename.h>
#pragma GCC diagnostic pop

namespace flint {
namespace gui {

std::string GetFnStrFromWxFileName(const wxFileName &fileName);

boost::filesystem::path GetPathFromWxFileName(const wxFileName &fileName);

boost::filesystem::path GetPathFromWxString(const wxString &str);

}
}

#endif
