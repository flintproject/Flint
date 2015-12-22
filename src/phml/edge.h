/* -*- Mode: C++; tab-width: 4; indent-tabs-mode: t; c-basic-offset: 4 -*- vim:set ts=4 sw=4 sts=4 noet: */
#ifndef FLINT_PHML_EDGE_H_
#define FLINT_PHML_EDGE_H_

#include <libxml/globals.h>
#include <libxml/xmlstring.h>

namespace flint {
namespace phml {

class Edge {
public:
	enum Type {
		kFunctional,
		kForwarding,
	};

	Edge(const Edge &) = delete;
	Edge &operator=(const Edge &) = delete;

	explicit Edge(Type type)
		: type_(type)
		, tail_module_id_()
		, tail_port_id_()
		, head_module_id_()
		, head_port_id_()
	{}

	~Edge() {
		if (tail_module_id_) xmlFree(tail_module_id_);
		if (tail_port_id_) xmlFree(tail_port_id_);
		if (head_module_id_) xmlFree(head_module_id_);
		if (head_port_id_) xmlFree(head_port_id_);
	}

	const xmlChar *tail_module_id() const {return tail_module_id_;}
	void set_tail_module_id(xmlChar *tail_module_id) {tail_module_id_ = tail_module_id;}
	const xmlChar *tail_port_id() const {return tail_port_id_;}
	void set_tail_port_id(xmlChar *tail_port_id) {tail_port_id_ = tail_port_id;}
	const xmlChar *head_module_id() const {return head_module_id_;}
	void set_head_module_id(xmlChar *head_module_id) {head_module_id_ = head_module_id;}
	const xmlChar *head_port_id() const {return head_port_id_;}
	void set_head_port_id(xmlChar *head_port_id) {head_port_id_ = head_port_id;}

	boost::uuids::uuid GetUuidOfTailModuleId() const {
		boost::uuids::string_generator gen;
		return gen((const char *)tail_module_id_);
	}

	boost::uuids::uuid GetUuidOfHeadModuleId() const {
		boost::uuids::string_generator gen;
		return gen((const char *)head_module_id_);
	}

private:
	Type type_;
	xmlChar *tail_module_id_;
	xmlChar *tail_port_id_;
	xmlChar *head_module_id_;
	xmlChar *head_port_id_;
};

}
}

#endif
