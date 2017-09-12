/* -*- Mode: C++; tab-width: 4; indent-tabs-mode: t; c-basic-offset: 4 -*- vim:set ts=4 sw=4 sts=4 noet: */
#ifndef FLINT_EXEC_H_
#define FLINT_EXEC_H_

#define BOOST_FILESYSTEM_NO_DEPRECATED
#include <boost/filesystem.hpp>

#include "sqlite3.h"

#include "cli.pb.h"

namespace flint {

namespace ctrl {
struct Argument;
}

namespace exec {

/*
 * Return true in case of success, false otherwise.
 */
bool Exec(const cli::ExecOption &option, const boost::filesystem::path &dir,
		  ctrl::Argument *arg);

}
}

#endif
