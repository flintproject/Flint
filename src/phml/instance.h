/* -*- Mode: C++; tab-width: 4; indent-tabs-mode: t; c-basic-offset: 4 -*- vim:set ts=4 sw=4 sts=4 noet: */
#ifndef FLINT_PHML_INSTANCE_H_
#define FLINT_PHML_INSTANCE_H_

#include <libxml/globals.h>
#include <libxml/xmlstring.h>

#include "sqlite3.h"

namespace flint {
namespace phml {

class Instance {
public:
	Instance(const Instance &) = delete;
	Instance &operator=(const Instance &) = delete;

	Instance()
		: module_id_(nullptr)
		, template_id_(nullptr)
		, label_(nullptr)
		, rowid_()
	{}

	~Instance() {
		if (module_id_) xmlFree(module_id_);
		if (template_id_) xmlFree(template_id_);
		if (label_) xmlFree(label_);
	}

	const xmlChar *module_id() const {return module_id_;}
	const xmlChar *template_id() const {return template_id_;}
	const xmlChar *label() const {return label_;}
	void set_module_id(xmlChar *module_id) {module_id_ = module_id;}
	void set_template_id(xmlChar *template_id) {template_id_ = template_id;}
	void set_label(xmlChar *label) {label_ = label;}

	sqlite3_int64 rowid() const {return rowid_;}
	void set_rowid(sqlite3_int64 rowid) {rowid_ = rowid;}

	boost::uuids::uuid GetUuidOfModuleId() const {
		boost::uuids::string_generator gen;
		return gen(reinterpret_cast<const char *>(module_id_));
	}

	boost::uuids::uuid GetUuidOfTemplateId() const {
		boost::uuids::string_generator gen;
		return gen(reinterpret_cast<const char *>(template_id_));
	}

	bool IsSaved() const {
		return rowid_ != 0;
	}

private:
	xmlChar *module_id_;
	xmlChar *template_id_;
	xmlChar *label_;
	sqlite3_int64 rowid_;
};

}
}

#endif
