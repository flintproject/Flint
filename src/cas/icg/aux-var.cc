/* -*- Mode: C++; tab-width: 4; indent-tabs-mode: t; c-basic-offset: 4 -*- vim:set ts=4 sw=4 sts=4 noet: */
#include "cas/icg.h"

#ifdef HAVE_CONFIG_H
#include "config.h"
#endif

#include <cassert>
#include <cstdio>
#include <iostream>

#include "cas/printer.h"
#include "db/ast-inserter.h"

using std::cerr;
using std::endl;

namespace flint {
namespace cas {
namespace icg {

AuxVar::AuxVar(Printer *printer, sqlite3 *db)
	: printer_(printer)
	, inserter_(new db::AstInserter(db))
{
	assert(printer != nullptr);
}

AuxVar::~AuxVar() = default;

bool AuxVar::Insert(const boost::uuids::uuid &uuid,
					const Def &def)
{
	std::ostringstream oss;
	oss << def.name() << "#0";
	std::string name = oss.str();
	printer_->set_uuid(uuid);
	boost::apply_visitor(*printer_, def.rhs());
	std::string math = printer_->GetAndClearString();
	return inserter_->Insert(uuid, name.c_str(), math.c_str());
}

bool AuxVar::Insert(const boost::uuids::uuid &uuid,
					const Ode &ode)
{
	std::ostringstream oss;
	oss << ode.name() << "#0";
	std::string name0 = oss.str();
	return inserter_->Insert(uuid, name0.c_str(), ode.name().c_str());
}

}
}
}
