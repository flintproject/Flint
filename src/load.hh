/* -*- Mode: C++; tab-width: 4; indent-tabs-mode: t; c-basic-offset: 4 -*- vim:set ts=4 sw=4 sts=4 noet: */
#ifndef FLINT_LOAD_HH_
#define FLINT_LOAD_HH_

#include "file.hh"

namespace load {

enum ConfigMode {
	kExec,
	kRun
};

/*
 * Generate a script for loading a model.
 * Return true in case of success, otherwise false.
 */
bool Config(file::Format format, ConfigMode mode, int dir = 0);

}

#endif
