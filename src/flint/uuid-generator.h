/* -*- Mode: C++; tab-width: 4; indent-tabs-mode: t; c-basic-offset: 4 -*- vim:set ts=4 sw=4 sts=4 noet: */
#ifndef FLINT_UUID_GENERATOR_H_
#define FLINT_UUID_GENERATOR_H_

#include <boost/uuid/uuid.hpp>

namespace flint {

/*
 * Generate Version 5 (SHA1) name-based UUID with the namespace obtained
 * from CellML namespace URL.
 */
boost::uuids::uuid GenerateUuidForCellml(const char *name);

}

#endif
