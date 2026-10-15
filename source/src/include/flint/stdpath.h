/* -*- Mode: C++; tab-width: 4; indent-tabs-mode: t; c-basic-offset: 4 -*- vim:set ts=4 sw=4 sts=4 noet: */
#ifndef FLINT_STDPATH_H_
#define FLINT_STDPATH_H_

#include <boost/filesystem.hpp>
#include <filesystem>

namespace flint {

inline std::filesystem::path ToStdPath(const boost::filesystem::path &p)
{
	return std::filesystem::path(p.native());
}

}

#endif
