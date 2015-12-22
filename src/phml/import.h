/* -*- Mode: C++; tab-width: 4; indent-tabs-mode: t; c-basic-offset: 4 -*- vim:set ts=4 sw=4 sts=4 noet: */
#ifndef FLINT_PHML_IMPORT_H_
#define FLINT_PHML_IMPORT_H_

#include <boost/uuid/uuid.hpp>

#include <libxml/globals.h>
#include <libxml/xmlstring.h>

#include "sqlite3.h"

namespace flint {
namespace phml {

class Import {
public:
	enum Format {
		kUnspecifiedFormat,
		kSbml
	};

	Import(const Import &) = delete;
	Import &operator=(const Import &) = delete;

	Import()
		: type_(nullptr)
		, format_(kUnspecifiedFormat)
		, iref_(nullptr)
		, xref_(nullptr)
		, zref_(nullptr)
	{}

	~Import() {
		if (type_) xmlFree(type_);
		if (iref_) xmlFree(iref_);
		if (xref_) xmlFree(xref_);
		if (zref_) xmlFree(zref_);
	}

	const xmlChar *type() const {return type_;}
	void set_type(xmlChar *type) {type_ = type;}
	Format format() const {return format_;}
	void set_format(Format format) {format_ = format;}
	const xmlChar *iref() const {return iref_;}
	void set_iref(xmlChar * iref) {iref_ = iref;}
	const xmlChar *xref() const {return xref_;}
	void set_xref(xmlChar * xref) {xref_ = xref;}
	const xmlChar *zref() const {return zref_;}
	void set_zref(xmlChar * zref) {zref_ = zref;}

private:
	xmlChar *type_;
	Format format_;
	xmlChar *iref_;
	xmlChar *xref_;
	xmlChar *zref_;
};

}

bool DumpImport(sqlite3 *db, const boost::uuids::uuid &uuid);

}

#endif
