/* -*- Mode: C++; tab-width: 4; indent-tabs-mode: t; c-basic-offset: 4 -*- vim:set ts=4 sw=4 sts=4 noet: */
#ifndef FLINT_ISDSTRIP_H_
#define FLINT_ISDSTRIP_H_

#include <cstdint>
#include <iostream>
#include <vector>

namespace flint {
namespace isdstrip {

/*
 * `num_columns` can be null.
 */
bool ExtractConstantColumns(const char *input,
							std::uint32_t *num_columns,
							std::vector<std::uint32_t> *cv);

int Filter(const char *input,
		   const std::vector<std::uint32_t> &cv,
		   std::ostream *os);

}
}

#endif
