/* -*- Mode: C++; tab-width: 4; indent-tabs-mode: t; c-basic-offset: 4 -*- vim:set ts=4 sw=4 sts=4 noet: */
#include "phml/graph-reader.h"

#include <cassert>
#include <cstdio>
#include <iostream>
#include <memory>

#include "flint/utf8string.h"
#include "mathml/math_dumper.h"
#include "phml/arc.h"
#include "phml/database-driver.h"
#include "phml/event-condition.h"
#include "phml/node.h"
#include "phml/pq.h"

namespace flint {
namespace phml {

GraphReader::GraphReader(const PQ *pq, xmlTextReaderPtr &text_reader,
						 DatabaseDriver *driver)
	: pq_(pq)
	, text_reader_(text_reader)
	, driver_(driver)
{}

int GraphReader::Read()
{
	int i = xmlTextReaderRead(text_reader_);
	while (i > 0) {
		int type = xmlTextReaderNodeType(text_reader_);
		if (type == XML_READER_TYPE_ELEMENT) {
			const xmlChar *local_name = xmlTextReaderConstLocalName(text_reader_);
			if (xmlStrEqual(local_name, reinterpret_cast<const xmlChar *>("graph"))) {
				i = ReadGraph();
				if (i <= 0) return i;
				continue;
			} else {
				std::cerr << "<graph> is expected, but: " << local_name << std::endl;
				return -2;
			}
		} else if (type == XML_READER_TYPE_END_ELEMENT) {
			const xmlChar *local_name = xmlTextReaderConstLocalName(text_reader_);
			if (xmlStrEqual(local_name, reinterpret_cast<const xmlChar *>("definition"))) {
				return xmlTextReaderRead(text_reader_);
			}
		}
		i = xmlTextReaderRead(text_reader_);
	}
	return i;
}

int GraphReader::Handle(int i)
{
	return i;
}

int GraphReader::ReadGraph()
{
	int i = xmlTextReaderRead(text_reader_);
	while (i > 0) {
		int type = xmlTextReaderNodeType(text_reader_);
		if (type == XML_READER_TYPE_ELEMENT) {
			const xmlChar *local_name = xmlTextReaderConstLocalName(text_reader_);
			if (xmlStrEqual(local_name, reinterpret_cast<const xmlChar *>("event-condition"))) {
				i = ReadEventCondition();
				if (i <= 0)
					return i;
				continue;
			} else if (xmlStrEqual(local_name, reinterpret_cast<const xmlChar *>("node-set"))) {
				i = ReadNodeSet();
				if (i <= 0) return i;
				continue;
			} else if (xmlStrEqual(local_name, reinterpret_cast<const xmlChar *>("arc-set"))) {
				i = ReadArcSet();
				if (i <= 0) return i;
				continue;
			} else {
				std::cerr << "unknown child of <graph>: " << local_name << std::endl;
				return -2;
			}
		} else if (type == XML_READER_TYPE_END_ELEMENT) {
			const xmlChar *local_name = xmlTextReaderConstLocalName(text_reader_);
			if (xmlStrEqual(local_name, reinterpret_cast<const xmlChar *>("graph"))) {
				return xmlTextReaderRead(text_reader_);
			}
		}
		i = xmlTextReaderRead(text_reader_);
	}
	return i;
}

int GraphReader::ReadEventCondition()
{
	EventCondition ec;
	int i = xmlTextReaderRead(text_reader_);
	while (i > 0) {
		int type = xmlTextReaderNodeType(text_reader_);
		if (type == XML_READER_TYPE_ELEMENT) {
			const xmlChar *local_name = xmlTextReaderConstLocalName(text_reader_);
			if (xmlStrEqual(local_name, reinterpret_cast<const xmlChar *>("math"))) {
				mathml::MathDumper math_dumper(text_reader_, &ec.stream());
				i = math_dumper.Read(&ec);
				if (i <= 0)
					return i;
				continue;
			} else {
				std::cerr << "unknown child of <event-condition>: " << local_name << std::endl;
				return -2;
			}
		} else if (type == XML_READER_TYPE_END_ELEMENT) {
			const xmlChar *local_name = xmlTextReaderConstLocalName(text_reader_);
			if (xmlStrEqual(local_name, reinterpret_cast<const xmlChar *>("event-condition"))) {
				if (!driver_->SaveEventCondition(pq_, ec))
					return -2;
				return xmlTextReaderRead(text_reader_);
			}
		}
		i = xmlTextReaderRead(text_reader_);
	}
	return i;
}

int GraphReader::ReadNodeSet()
{
	int i = xmlTextReaderRead(text_reader_);
	while (i > 0) {
		int type = xmlTextReaderNodeType(text_reader_);
		if (type == XML_READER_TYPE_ELEMENT) {
			const xmlChar *local_name = xmlTextReaderConstLocalName(text_reader_);
			if (xmlStrEqual(local_name, reinterpret_cast<const xmlChar *>("node"))) {
				i = ReadNode();
				if (i <= 0) return i;
				continue;
			} else {
				std::cerr << "unknown child of <node-set>: " << local_name << std::endl;
				return -2;
			}
		} else if (type == XML_READER_TYPE_END_ELEMENT) {
			const xmlChar *local_name = xmlTextReaderConstLocalName(text_reader_);
			if (xmlStrEqual(local_name, reinterpret_cast<const xmlChar *>("node-set"))) {
				return xmlTextReaderRead(text_reader_);
			}
		}
		i = xmlTextReaderRead(text_reader_);
	}
	return i;
}

int GraphReader::ReadNode()
{
	int node_id = 0;
	int i;
	while ( (i = xmlTextReaderMoveToNextAttribute(text_reader_)) > 0) {
		if (xmlTextReaderIsNamespaceDecl(text_reader_)) continue;

		const xmlChar *local_name = xmlTextReaderConstLocalName(text_reader_);
		if (xmlStrEqual(local_name, reinterpret_cast<const xmlChar *>("node-id"))) {
			const xmlChar *value = xmlTextReaderConstValue(text_reader_);
			node_id = atoi((const char *)value);
			if (node_id <= 0) {
				std::cerr << "invalid value of node-id of <node>: " << value << std::endl;
				return -2;
			}
		} else {
			std::cerr << "unknown attribute of <node>: " << local_name << std::endl;
			return -2;
		}
	}
	if (node_id == 0) {
		std::cerr << "missing node-id of <node>" << std::endl;
		return -2;
	}
	std::unique_ptr<Node> node(new Node(node_id));
	i = xmlTextReaderRead(text_reader_);
	while (i > 0) {
		int type = xmlTextReaderNodeType(text_reader_);
		if (type == XML_READER_TYPE_ELEMENT) {
			const xmlChar *local_name = xmlTextReaderConstLocalName(text_reader_);
			if (xmlStrEqual(local_name, reinterpret_cast<const xmlChar *>("name"))) {
				i = ReadNodeName(node.get());
				if (i <= 0) return i;
				continue;
			} else if (xmlStrEqual(local_name, reinterpret_cast<const xmlChar *>("description"))) {
				// ignore description
				i = xmlTextReaderNext(text_reader_);
				continue;
			} else {
				std::cerr << "unknown child of <node>: " << local_name << std::endl;
				return -2;
			}
		} else if (type == XML_READER_TYPE_END_ELEMENT) {
			const xmlChar *local_name = xmlTextReaderConstLocalName(text_reader_);
			if (xmlStrEqual(local_name, reinterpret_cast<const xmlChar *>("node"))) {
				if (!node->name()) {
					std::cerr << "missing <name> of <node>" << std::endl;
					return -2;
				}
				if (!driver_->SaveNode(pq_, node.get())) return -2;
				return xmlTextReaderRead(text_reader_);
			}
		}
		i = xmlTextReaderRead(text_reader_);
	}
	return i;
}

int GraphReader::ReadNodeName(Node *node)
{
	xmlChar *s = xmlTextReaderReadString(text_reader_);
	assert(s);

	xmlChar *name;
	if (!Trim(s, &name)) {
		xmlFree(s);
		return -2;
	}
	if (xmlStrlen(name) == 0) {
		std::cerr << "node name is empty" << std::endl;
		xmlFree(s);
		return -2;
	}
	if (ContainNonGraphic(name)) {
		std::cerr << "node name contains invalid character: \"" << name << "\"" << std::endl;
		xmlFree(s);
		return -2;
	}

	node->set_name(xmlStrdup(name));
	xmlFree(s);
	return xmlTextReaderNext(text_reader_);
}

int GraphReader::ReadArcSet()
{
	int i = xmlTextReaderRead(text_reader_);
	while (i > 0) {
		int type = xmlTextReaderNodeType(text_reader_);
		if (type == XML_READER_TYPE_ELEMENT) {
			const xmlChar *local_name = xmlTextReaderConstLocalName(text_reader_);
			if (xmlStrEqual(local_name, reinterpret_cast<const xmlChar *>("arc"))) {
				i = ReadArc();
				if (i <= 0) return i;
				continue;
			} else {
				std::cerr << "unknown child of <arc-set>: " << local_name << std::endl;
				return -2;
			}
		} else if (type == XML_READER_TYPE_END_ELEMENT) {
			const xmlChar *local_name = xmlTextReaderConstLocalName(text_reader_);
			if (xmlStrEqual(local_name, reinterpret_cast<const xmlChar *>("arc-set"))) {
				return xmlTextReaderRead(text_reader_);
			}
		}
		i = xmlTextReaderRead(text_reader_);
	}
	return i;
}

int GraphReader::ReadArc()
{
	int arc_id = 0;
	int head_node_id = 0;
	int tail_node_id = 0;
	int i;
	while ( (i = xmlTextReaderMoveToNextAttribute(text_reader_)) > 0) {
		if (xmlTextReaderIsNamespaceDecl(text_reader_)) continue;

		const xmlChar *local_name = xmlTextReaderConstLocalName(text_reader_);
		if (xmlStrEqual(local_name, reinterpret_cast<const xmlChar *>("arc-id"))) {
			const xmlChar *value = xmlTextReaderConstValue(text_reader_);
			arc_id = atoi((const char *)value);
			if (arc_id <= 0) {
				std::cerr << "invalid value of arc-id of <arc>: " << value << std::endl;
				return -2;
			}
		} else if (xmlStrEqual(local_name, reinterpret_cast<const xmlChar *>("head-node-id"))) {
			const xmlChar *value = xmlTextReaderConstValue(text_reader_);
			head_node_id = std::atoi(reinterpret_cast<const char *>(value));
			if (arc_id <= 0) {
				std::cerr << "invalid value of head-node-id of <arc>: " << value << std::endl;
				return -2;
			}
		} else if (xmlStrEqual(local_name, reinterpret_cast<const xmlChar *>("tail-node-id"))) {
			const xmlChar *value = xmlTextReaderConstValue(text_reader_);
			tail_node_id = std::atoi(reinterpret_cast<const char *>(value));
			if (arc_id <= 0) {
				std::cerr << "invalid value of tail-node-id of <arc>: " << value << std::endl;
				return -2;
			}
		} else {
			std::cerr << "unknown attribute of <arc>: " << local_name << std::endl;
			return -2;
		}
	}
	if (arc_id == 0) {
		std::cerr << "missing arc-id of <arc>" << std::endl;
		return -2;
	}
	if (head_node_id == 0) {
		std::cerr << "missing head-node-id of <arc>" << std::endl;
		return -2;
	}
	if (tail_node_id == 0) {
		std::cerr << "missing tail-node-id of <arc>" << std::endl;
		return -2;
	}
	std::unique_ptr<Arc> arc(new Arc(arc_id, tail_node_id, head_node_id));
	i = xmlTextReaderRead(text_reader_);
	while (i > 0) {
		int type = xmlTextReaderNodeType(text_reader_);
		if (type == XML_READER_TYPE_ELEMENT) {
			const xmlChar *local_name = xmlTextReaderConstLocalName(text_reader_);
			if (xmlStrEqual(local_name, reinterpret_cast<const xmlChar *>("transition"))) {
				i = ReadTransition(arc.get());
				if (i <= 0) return i;
				continue;
			} else if (xmlStrEqual(local_name, reinterpret_cast<const xmlChar *>("description"))) {
				// ignore description
				i = xmlTextReaderNext(text_reader_);
				continue;
			} else {
				std::cerr << "unknown child of <arc>: " << local_name << std::endl;
				return -2;
			}
		} else if (type == XML_READER_TYPE_END_ELEMENT) {
			const xmlChar *local_name = xmlTextReaderConstLocalName(text_reader_);
			if (xmlStrEqual(local_name, reinterpret_cast<const xmlChar *>("arc"))) {
				if (arc->type() == Arc::kUnspecified) {
					std::cerr << "missing transition of <arc>: " << arc_id << std::endl;
					return -2;
				}
				if (!driver_->SaveArc(pq_, arc.get())) return -2;
				return xmlTextReaderRead(text_reader_);
			}
		}
		i = xmlTextReaderRead(text_reader_);
	}
	return i;
}

int GraphReader::ReadTransition(Arc *arc)
{
	int i;
	while ( (i = xmlTextReaderMoveToNextAttribute(text_reader_)) > 0) {
		if (xmlTextReaderIsNamespaceDecl(text_reader_)) continue;

		const xmlChar *local_name = xmlTextReaderConstLocalName(text_reader_);
		if (xmlStrEqual(local_name, reinterpret_cast<const xmlChar *>("type"))) {
			const xmlChar *value = xmlTextReaderConstValue(text_reader_);
			if (xmlStrEqual(value, reinterpret_cast<const xmlChar *>("condition"))) {
				arc->set_type(Arc::kCondition);
			} else if (xmlStrEqual(value, reinterpret_cast<const xmlChar *>("probability"))) {
				arc->set_type(Arc::kProbability);
			} else {
				std::cerr << "unknown type of <transition>: " << value << std::endl;
				return -2;
			}
		} else {
			std::cerr << "unknown attribute of <transition>: " << local_name << std::endl;
			return -2;
		}
	}
	if (arc->type() == Arc::kUnspecified) {
		std::cerr << "missing type of <transition>" << std::endl;
		return -2;
	}
	i = xmlTextReaderRead(text_reader_);
	while (i > 0) {
		int type = xmlTextReaderNodeType(text_reader_);
		if (type == XML_READER_TYPE_ELEMENT) {
			const xmlChar *local_name = xmlTextReaderConstLocalName(text_reader_);
			if (xmlStrEqual(local_name, reinterpret_cast<const xmlChar *>("math"))) {
				mathml::MathDumper math_dumper(text_reader_, &arc->stream());
				i = math_dumper.Read(this);
				if (i <= 0) return i;
				continue;
			} else {
				std::cerr << "unknown child of <transition>: " << local_name << std::endl;
				return -2;
			}
		} else if (type == XML_READER_TYPE_END_ELEMENT) {
			const xmlChar *local_name = xmlTextReaderConstLocalName(text_reader_);
			if (xmlStrEqual(local_name, reinterpret_cast<const xmlChar *>("transition"))) {
				return xmlTextReaderRead(text_reader_);
			}
		}
		i = xmlTextReaderRead(text_reader_);
	}
	return i;
}

}
}
