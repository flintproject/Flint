/* -*- Mode: C++; tab-width: 4; indent-tabs-mode: t; c-basic-offset: 4 -*- vim:set ts=4 sw=4 sts=4 noet: */
#ifndef FLINT_UTF8PATH_H_
#define FLINT_UTF8PATH_H_

#define BOOST_FILESYSTEM_NO_DEPRECATED
#include <boost/filesystem.hpp>

namespace flint {

/*
 * Get a path from a filename encoded in UTF-8.
 * Returns the empty path if failed.
 */
boost::filesystem::path GetPathFromUtf8(const char *utf8);

/*
 * Get the filename encoded in UTF-8 from a path.
 * If succeeded, the return value is a null-terminated string, which the client
 * code is responsible for freeing by delete [] after use.
 * Returns null in case of failure.
 */
char *GetUtf8FromPath(const boost::filesystem::path &path);

}

#endif
