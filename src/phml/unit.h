/* -*- Mode: C++; tab-width: 4; indent-tabs-mode: t; c-basic-offset: 4 -*- vim:set ts=4 sw=4 sts=4 noet: */
#ifndef FLINT_PHML_UNIT_H_
#define FLINT_PHML_UNIT_H_

#include <libxml/globals.h>
#include <libxml/xmlstring.h>

#include "sqlite3.h"

namespace flint {
namespace phml {

class Unit {
public:
	Unit(const Unit &) = delete;
	Unit &operator=(const Unit &) = delete;

	explicit Unit(int unit_id)
		: unit_id_(unit_id)
		, name_(nullptr)
		, rowid_()
	{}

	~Unit() {
		if (name_) xmlFree(name_);
	}

	int unit_id() const {return unit_id_;}
	const xmlChar *name() const {return name_;}
	void set_name(xmlChar *name) {name_ = name;}
	sqlite3_int64 rowid() const {return rowid_;}
	void set_rowid(sqlite3_int64 rowid) {rowid_ = rowid;}

private:
	int unit_id_;
	xmlChar *name_;
	sqlite3_int64 rowid_;
};

}
}

#endif
