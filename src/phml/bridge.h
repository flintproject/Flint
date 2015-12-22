/* -*- Mode: C++; tab-width: 4; indent-tabs-mode: t; c-basic-offset: 4 -*- vim:set ts=4 sw=4 sts=4 noet: */
#ifndef FLINT_PHML_BRIDGE_H_
#define FLINT_PHML_BRIDGE_H_

#include <libxml/globals.h>
#include <libxml/xmlstring.h>

namespace flint {
namespace phml {

class Bridge {
public:
	Bridge(const Bridge &) = delete;
	Bridge &operator=(const Bridge &) = delete;

	Bridge()
		: sub_type_(nullptr)
		, direction_(nullptr)
		, connector_(nullptr)
	{}

	~Bridge() {
		if (sub_type_) xmlFree(sub_type_);
		if (direction_) xmlFree(direction_);
		if (connector_) xmlFree(connector_);
	}

	const xmlChar *sub_type() const {return sub_type_;}
	void set_sub_type(xmlChar *sub_type) {sub_type_ = sub_type;}
	const xmlChar *direction() const {return direction_;}
	void set_direction(xmlChar *direction) {direction_ = direction;}
	const xmlChar *connector() const {return connector_;}
	void set_connector(xmlChar *connector) {connector_ = connector;}

private:
	xmlChar *sub_type_;
	xmlChar *direction_;
	xmlChar *connector_;
};

}
}

#endif
