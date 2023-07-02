/* -*- Mode: C++; tab-width: 4; indent-tabs-mode: t; c-basic-offset: 4 -*- vim:set ts=4 sw=4 sts=4 noet: */
#include "compiler.h"

#include "cas/dimension.h"
#include "compiler/bcc.h"
#include "compiler/sort.h"
#include "compiler/tac.h"
#include "db/driver.h"
#include "flint/bc.h"
#include "method.h"

namespace flint {
namespace compiler {

Compiler::Compiler(const cas::DimensionAnalyzer *da)
	: da_(da)
{}

Compiler::~Compiler() = default;

Bytecode *Compiler::Compile(sqlite3 *db, const char *table, Method method)
{
	db::Driver tmp(":memory:");
	switch (method) {
	case Method::kAssign:
		if (!method::Assign(db, table, tmp.db()))
			return nullptr;
		break;
	case Method::kEuler:
		if (!method::Euler(db, table, tmp.db()))
			return nullptr;
		break;
	case Method::kEvent:
		if (!method::Event(db, table, tmp.db()))
			return nullptr;
		break;
	case Method::kRk4:
		if (!method::Rk4(db, table, tmp.db()))
			return nullptr;
		break;
	case Method::kArk:
		assert(false); // TODO
		break;
	case Method::kEulerMaruyama:
		if (!method::EulerMaruyama(db, table, tmp.db()))
			return nullptr;
		break;
	}
	return GenerateBytecode(tmp.db());
}

Bytecode *Compiler::GenerateBytecode(sqlite3 *db)
{
	if (!compiler::sort::Sort(db))
		return nullptr;
	if (!compiler::tac::Tac(da_, db))
		return nullptr;
	return compiler::bcc::Bcc(db);
}

}
}
