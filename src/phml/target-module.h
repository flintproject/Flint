/* -*- Mode: C++; tab-width: 4; indent-tabs-mode: t; c-basic-offset: 4 -*- vim:set ts=4 sw=4 sts=4 noet: */
#ifndef FLINT_PHML_TARGET_MODULE_H_
#define FLINT_PHML_TARGET_MODULE_H_

#include <libxml/globals.h>
#include <libxml/xmlstring.h>

#include "sqlite3.h"

namespace flint {
namespace phml {

class TargetModule {
public:
	TargetModule(const TargetModule &) = delete;
	TargetModule &operator=(const TargetModule &) = delete;

	explicit TargetModule(xmlChar *module_id)
		: module_id_(module_id)
		, rowid_()
	{}

	~TargetModule() {
		if (module_id_) xmlFree(module_id_);
	}

	const xmlChar *module_id() const {return module_id_;}

	sqlite3_int64 rowid() const {return rowid_;}
	void set_rowid(sqlite3_int64 rowid) {rowid_ = rowid;}

	boost::uuids::uuid GetUuid() const {
		boost::uuids::string_generator gen;
		return gen((const char *)module_id_);
	}

	bool IsSaved() const {
		return rowid_ != 0;
	}

private:
	xmlChar *module_id_;
	sqlite3_int64 rowid_;
};

}
}

#endif
