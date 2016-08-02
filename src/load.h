/* -*- Mode: C++; tab-width: 4; indent-tabs-mode: t; c-basic-offset: 4 -*- vim:set ts=4 sw=4 sts=4 noet: */
#ifndef FLINT_LOAD_H_
#define FLINT_LOAD_H_

#include <vector>

namespace flint {
namespace load {

enum ConfigMode {
	kExec,
	kOpen,
	kRun
};

/*
 * Load the given model.
 * given_file is encoded in UTF-8.
 * Return true in case of success, otherwise false.
 */
bool Load(const char *given_file, ConfigMode mode, int dir,
		  std::vector<double> *data);

}
}

#endif
