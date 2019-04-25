/* -*- Mode: C++; tab-width: 4; indent-tabs-mode: t; c-basic-offset: 4 -*- vim:set ts=4 sw=4 sts=4 noet: */
#ifndef FLINT_COMPILER_H_
#define FLINT_COMPILER_H_

#include "flint/bc.h"
#include "sqlite3.h"

namespace flint {

struct Bytecode;

namespace cas {
class DimensionAnalyzer;
}

namespace compiler {

enum class Method {
	kAssign,
	kEuler,
	kEvent,
	kRk4,
	kArk,
	kEulerMaruyama
};

class Compiler {
public:
	explicit Compiler(const cas::DimensionAnalyzer *da);

	~Compiler();

	/*
	 * Note that db is for read only.
	 * Returns nullptr in case of failure.
	 */
	Bytecode *Compile(sqlite3 *db, const char *table, Method method);

	/*
	 * Note that db is for read only.
	 * Returns nullptr in case of failure.
	 */
	Bytecode *GenerateBytecode(sqlite3 *db);

private:
	const cas::DimensionAnalyzer *da_;
};

}
}

#endif
