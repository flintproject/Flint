/* -*- Mode: C++; tab-width: 4; indent-tabs-mode: t; c-basic-offset: 4 -*- vim:set ts=4 sw=4 sts=4 noet: */
#ifndef FLINT_LOAD_H_
#define FLINT_LOAD_H_

#include <vector>

#define BOOST_FILESYSTEM_NO_DEPRECATED
#include <boost/filesystem.hpp>

namespace flint {

namespace task {
struct Task;
}

namespace load {

enum ConfigMode {
	kExec,
	kOpen,
	kRun
};

/*
 * Load the given model.
 * given_file is encoded in UTF-8.
 * Return nullptr in case of failure.
 */
task::Task *Load(const char *given_file, ConfigMode mode,
				 const boost::filesystem::path &dir,
				 std::vector<double> *data);

}
}

#endif
