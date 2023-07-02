/* -*- Mode: C++; tab-width: 4; indent-tabs-mode: t; c-basic-offset: 4 -*- vim:set ts=4 sw=4 sts=4 noet: */
#ifndef FLINT_PHML_DEFINITION_H_
#define FLINT_PHML_DEFINITION_H_

#include <libxml/globals.h>
#include <libxml/xmlstring.h>

namespace flint {
namespace phml {

class Definition {
public:
	Definition(const Definition &) = delete;
	Definition &operator=(const Definition &) = delete;

	Definition()
		: type_(nullptr)
		, sub_type_(nullptr)
		, format_(nullptr)
	{}

	~Definition() {
		if (type_) xmlFree(type_);
		if (sub_type_) xmlFree(sub_type_);
		if (format_) xmlFree(format_);
	}

	const xmlChar *type() const {return type_;}
	const xmlChar *sub_type() const {return sub_type_;}
	const xmlChar *format() const {return format_;}
	void set_type(xmlChar *type) {type_ = type;}
	void set_sub_type(xmlChar *sub_type) {sub_type_ = sub_type;}
	void set_format(xmlChar *format) {format_ = format;}

private:
	xmlChar *type_;
	xmlChar *sub_type_;
	xmlChar *format_;
};

}
}

#endif
