/* -*- Mode: C++; tab-width: 4; indent-tabs-mode: t; c-basic-offset: 4 -*- vim:set ts=4 sw=4 sts=4 noet: */
#ifndef FLINT_PHML_REFERENCE_H_
#define FLINT_PHML_REFERENCE_H_

#include <libxml/globals.h>
#include <libxml/xmlstring.h>

namespace flint {
namespace phml {

class Reference {
public:
	Reference(const Reference &) = delete;
	Reference &operator=(const Reference &) = delete;

	Reference()
		: port_id_()
		, timeseries_id_()
		, element_id_(nullptr)
	{}

	~Reference() {
		if (element_id_) xmlFree(element_id_);
	}

	int port_id() const {return port_id_;}
	int timeseries_id() const {return timeseries_id_;}
	const xmlChar *element_id() const {return element_id_;}

	void set_port_id(int port_id) {port_id_ = port_id;}
	void set_timeseries_id(int timeseries_id) {timeseries_id_ = timeseries_id;}
	void set_element_id(xmlChar *element_id) {element_id_ = element_id;}

private:
	int port_id_;
	int timeseries_id_;
	xmlChar *element_id_;
};

}
}

#endif
