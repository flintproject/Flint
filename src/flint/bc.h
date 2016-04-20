/* -*- Mode: C++; tab-width: 4; indent-tabs-mode: t; c-basic-offset: 4 -*- vim:set ts=4 sw=4 sts=4 noet: */
#ifndef FLINT_BC_H_
#define FLINT_BC_H_

#include <string>

namespace flint {

namespace ct {
class DataFlowAnalyzer;
}

bool LoadBytecode(const std::string &file, int *nol, ct::DataFlowAnalyzer *dfa);

}

#endif
