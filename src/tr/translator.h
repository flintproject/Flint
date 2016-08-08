/* -*- Mode: C++; tab-width: 4; indent-tabs-mode: t; c-basic-offset: 4 -*- vim:set ts=4 sw=4 sts=4 noet: */
#ifndef FLINT_TR_TRANSLATOR_H_
#define FLINT_TR_TRANSLATOR_H_

#include <iostream>

#include "flint/ct.h"

namespace flint {

struct Bytecode;

namespace filter {
class Writer;
}

namespace tr {

class Translator : public ct::DataFlowAnalyzer {
public:
	Translator(const Translator &) = delete;
	Translator &operator=(const Translator &) = delete;

	Translator(const Layout *layout, int layer_size, Bytecode *bytecode,
			   std::ostream &os);

	void PrintHeader(int nol, size_t layer_size, double length, double step);

	void PrintFunctions();

	void PrintReductionFunctions();

	void PrintMain(const filter::Writer &writer);

private:
	std::ostream &os_;
};

}
}

#endif
