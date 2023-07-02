/* -*- Mode: C++; tab-width: 4; indent-tabs-mode: t; c-basic-offset: 4 -*- vim:set ts=4 sw=4 sts=4 noet: */
#ifndef FLINT_PHML_MODULE_H_
#define FLINT_PHML_MODULE_H_

#include <libxml/globals.h>
#include <libxml/xmlstring.h>

#include "sqlite3.h"

namespace flint {
namespace phml {

class Module {
public:
	Module(const Module &) = delete;
	Module &operator=(const Module &) = delete;

	Module()
		: module_id_(nullptr)
		, type_(nullptr)
		, name_(nullptr)
		, capsulated_by_(nullptr)
		, template_state_(nullptr)
		, rowid_()
	{}

	~Module() {
		if (module_id_) xmlFree(module_id_);
		if (type_) xmlFree(type_);
		if (name_) xmlFree(name_);
		if (capsulated_by_) xmlFree(capsulated_by_);
		if (template_state_) xmlFree(template_state_);
	}

	const xmlChar *module_id() const {return module_id_;}
	void set_module_id(xmlChar *module_id) {module_id_ = module_id;}
	const xmlChar *type() const {return type_;}
	void set_type(xmlChar *type) {type_ = type;}
	const xmlChar *name() const {return name_;}
	void set_name(xmlChar *name) {name_ = name;}
	const xmlChar *capsulated_by() const {return capsulated_by_;}
	void set_capsulated_by(xmlChar *capsulated_by) {capsulated_by_ = capsulated_by;}
	const xmlChar *template_state() const {return template_state_;}
	void set_template_state(xmlChar *template_state) {template_state_ = template_state;}

	sqlite3_int64 rowid() const {return rowid_;}
	void set_rowid(sqlite3_int64 rowid) {rowid_ = rowid;}

	boost::uuids::uuid GetUuid() const {
		boost::uuids::string_generator gen;
		return gen(reinterpret_cast<const char *>(module_id_));
	}

	boost::uuids::uuid GetCapsulatedBy() const {
		assert(capsulated_by_);
		boost::uuids::string_generator gen;
		return gen(reinterpret_cast<const char *>(capsulated_by_));
	}

	bool IsValid() const {
		return module_id_ && name_;
	}

	bool IsSaved() const {
		return rowid_ != 0;
	}

private:
	xmlChar *module_id_;
	xmlChar *type_;
	xmlChar *name_;
	xmlChar *capsulated_by_;
	xmlChar *template_state_;
	sqlite3_int64 rowid_;
};

}
}

#endif
