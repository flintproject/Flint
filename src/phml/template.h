/* -*- Mode: C++; tab-width: 4; indent-tabs-mode: t; c-basic-offset: 4 -*- vim:set ts=4 sw=4 sts=4 noet: */
#ifndef FLINT_PHML_TEMPLATE_H_
#define FLINT_PHML_TEMPLATE_H_

#include <libxml/globals.h>
#include <libxml/xmlstring.h>

namespace flint {
namespace phml {

class Template {
public:
	Template(const Template &) = delete;
	Template &operator=(const Template &) = delete;

	Template() : template_id_(NULL), ref_module_id_(NULL) {}

	~Template() {
		if (template_id_) xmlFree(template_id_);
		if (ref_module_id_) xmlFree(ref_module_id_);
	}

	const xmlChar *template_id() const {return template_id_;}
	void set_template_id(xmlChar *template_id) {template_id_ = template_id;}
	const xmlChar *ref_module_id() const {return ref_module_id_;}
	void set_ref_module_id(xmlChar *ref_module_id) {ref_module_id_ = ref_module_id;}

	boost::uuids::uuid GetUuidOfTemplateId() const {
		boost::uuids::string_generator gen;
		return gen((const char *)template_id_);
	}

	boost::uuids::uuid GetUuidOfRefModuleId() const {
		boost::uuids::string_generator gen;
		return gen((const char *)ref_module_id_);
	}

private:
	xmlChar *template_id_;
	xmlChar *ref_module_id_;
};

}
}

#endif
