/* -*- Mode: C++; tab-width: 4; indent-tabs-mode: t; c-basic-offset: 4 -*- vim:set ts=4 sw=4 sts=4 noet: */
#include "cellml.h"

#include "cellml/parser.h"
#include "cellml/translator.h"
#include "db.h"

namespace flint {
namespace cellml {

bool Read(sqlite3 *db)
{
	return ParseCellml(db) && TranslateCellml(db) && db::Flow(db);
}

}
}
