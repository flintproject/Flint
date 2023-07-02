/* -*- Mode: C++; tab-width: 4; indent-tabs-mode: t; c-basic-offset: 4 -*- vim:set ts=4 sw=4 sts=4 noet: */
#ifndef FLINT_WORKSPACE_H_
#define FLINT_WORKSPACE_H_

#include <cstddef>

#define BOOST_FILESYSTEM_NO_DEPRECATED
#include <boost/filesystem.hpp>

namespace flint {
namespace workspace {

bool CreateSparseFile(const boost::filesystem::path &filename, size_t size);

bool CreateSparseFileAtomically(const boost::filesystem::path &filename, size_t size);

}
}

#endif
