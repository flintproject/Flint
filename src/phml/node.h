/* -*- Mode: C++; tab-width: 4; indent-tabs-mode: t; c-basic-offset: 4 -*- vim:set ts=4 sw=4 sts=4 noet: */
#ifndef FLINT_PHML_NODE_H_
#define FLINT_PHML_NODE_H_

#include <libxml/globals.h>
#include <libxml/xmlstring.h>

namespace flint {
namespace phml {

class Node {
public:
	Node(const Node &) = delete;
	Node &operator=(const Node &) = delete;

	explicit Node(int node_id)
		: node_id_(node_id)
		, name_(nullptr)
	{}

	~Node() {
		if (name_) xmlFree(name_);
	}

	int node_id() const {return node_id_;}
	const xmlChar *name() const {return name_;}
	void set_name(xmlChar *name) {name_ = name;}

private:
	int node_id_;
	xmlChar *name_;
};

}
}

#endif
