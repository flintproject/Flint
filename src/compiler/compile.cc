/* -*- Mode: C++; tab-width: 4; indent-tabs-mode: t; c-basic-offset: 4 -*- vim:set ts=4 sw=4 sts=4 noet: */
#include "compiler.hh"

#include <cstdlib>
#include <cstdio>
#include <fstream>

#include "compiler/bcc.h"
#include "compiler/sort.h"
#include "compiler/tac.h"
#include "db/driver.h"
#include "method.hh"

using std::cerr;
using std::endl;
using std::strcmp;

namespace compiler {

bool Compile(sqlite3 *db, const char *table, const char *method, const char *output)
{
	db::Driver tmp("");
	if (strcmp("assign", method) == 0) {
		if (!method::Assign(db, table, tmp.db()))
			return false;
	} else if (strcmp("euler", method) == 0) {
		if (!method::Euler(db, table, tmp.db()))
			return false;
	} else if (strcmp("event", method) == 0) {
		if (!method::Event(db, table, tmp.db()))
			return false;
	} else if (strcmp("rk4", method) == 0) {
		if (!method::Rk4(db, table, tmp.db()))
			return false;
	} else {
		cerr << "unknown method: " << method << endl;
		return false;
	}
	if (!compiler::sort::Sort(tmp.db()))
		return false;
	if (!compiler::tac::Tac(tmp.db()))
		return false;
	std::ofstream ofs(output, std::ios::binary);
	if (!ofs) {
		cerr << "failed to open " << output << endl;
		return false;
	}
	if (!compiler::bcc::Bcc(tmp.db(), &ofs))
		return false;
	ofs.close();
	return true;
}

}
