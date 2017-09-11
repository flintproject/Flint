/* -*- Mode: C++; tab-width: 4; indent-tabs-mode: t; c-basic-offset: 4 -*- vim:set ts=4 sw=4 sts=4 noet: */
#ifndef FLINT_TR_H_
#define FLINT_TR_H_

#define BOOST_FILESYSTEM_NO_DEPRECATED
#include <boost/filesystem.hpp>

#include "cli.pb.h"

namespace flint {
namespace tr {

/*
 * Return true in case of success, false otherwise.
 */
bool Translate(const cli::RunOption &option, const boost::filesystem::path &dir);

}
}

#endif
