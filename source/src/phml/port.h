/* -*- Mode: C++; tab-width: 4; indent-tabs-mode: t; c-basic-offset: 4 -*- vim:set ts=4 sw=4 sts=4 noet: */
#ifndef FLINT_PHML_PORT_H_
#define FLINT_PHML_PORT_H_

#include <libxml/globals.h>
#include <libxml/xmlstring.h>

namespace flint {
namespace phml {

class Port {
public:
	Port(const Port &) = delete;
	Port &operator=(const Port &) = delete;

	Port()
		: port_id_(nullptr)
		, direction_(nullptr)
		, ref_pq_id_(nullptr)
		, multiple_(nullptr)
	{}

	~Port() {
		if (port_id_) xmlFree(port_id_);
		if (direction_) xmlFree(direction_);
		if (ref_pq_id_) xmlFree(ref_pq_id_);
		if (multiple_) xmlFree(multiple_);
	}

	const xmlChar *port_id() const {return port_id_;}
	void set_port_id(xmlChar *port_id) {port_id_ = port_id;}
	const xmlChar *direction() const {return direction_;}
	void set_direction(xmlChar *direction) {direction_ = direction;}
	const xmlChar *ref_pq_id() const {return ref_pq_id_;}
	void set_ref_pq_id(xmlChar *ref_pq_id) {ref_pq_id_ = ref_pq_id;}
	const xmlChar *multiple() const {return multiple_;}
	void set_multiple(xmlChar *multiple) {multiple_ = multiple;}

private:
	xmlChar *port_id_;
	xmlChar *direction_;
	xmlChar *ref_pq_id_;
	xmlChar *multiple_;
};

}
}

#endif
