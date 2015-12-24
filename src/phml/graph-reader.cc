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

using std::cerr;
using std::endl;

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
			if (xmlStrEqual(local_name, BAD_CAST "graph")) {
				i = ReadGraph();
				if (i <= 0) return i;
				continue;
			} else {
				cerr << "<graph> is expected, but: " << local_name << endl;
				return -2;
			}
		} else if (type == XML_READER_TYPE_END_ELEMENT) {
			const xmlChar *local_name = xmlTextReaderConstLocalName(text_reader_);
			if (xmlStrEqual(local_name, BAD_CAST "definition")) {
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
			if (xmlStrEqual(local_name, BAD_CAST "event-condition")) {
				i = ReadEventCondition();
				if (i <= 0)
					return i;
				continue;
			} else if (xmlStrEqual(local_name, BAD_CAST "node-set")) {
				i = ReadNodeSet();
				if (i <= 0) return i;
				continue;
			} else if (xmlStrEqual(local_name, BAD_CAST "arc-set")) {
				i = ReadArcSet();
				if (i <= 0) return i;
				continue;
			} else {
				cerr << "unknown child of <graph>: " << local_name << endl;
				return -2;
			}
		} else if (type == XML_READER_TYPE_END_ELEMENT) {
			const xmlChar *local_name = xmlTextReaderConstLocalName(text_reader_);
			if (xmlStrEqual(local_name, BAD_CAST "graph")) {
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
			if (xmlStrEqual(local_name, BAD_CAST "math")) {
				mathml::MathDumper math_dumper(text_reader_, &ec.stream());
				i = math_dumper.Read(&ec);
				if (i <= 0)
					return i;
				continue;
			} else {
				cerr << "unknown child of <event-condition>: " << local_name << endl;
				return -2;
			}
		} else if (type == XML_READER_TYPE_END_ELEMENT) {
			const xmlChar *local_name = xmlTextReaderConstLocalName(text_reader_);
			if (xmlStrEqual(local_name, BAD_CAST "event-condition")) {
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
			if (xmlStrEqual(local_name, BAD_CAST "node")) {
				i = ReadNode();
				if (i <= 0) return i;
				continue;
			} else {
				cerr << "unknown child of <node-set>: " << local_name << endl;
				return -2;
			}
		} else if (type == XML_READER_TYPE_END_ELEMENT) {
			const xmlChar *local_name = xmlTextReaderConstLocalName(text_reader_);
			if (xmlStrEqual(local_name, BAD_CAST "node-set")) {
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
		if (xmlStrEqual(local_name, BAD_CAST "node-id")) {
			const xmlChar *value = xmlTextReaderConstValue(text_reader_);
			node_id = atoi((const char *)value);
			if (node_id <= 0) {
				cerr << "invalid value of node-id of <node>: " << value << endl;
				return -2;
			}
		} else {
			cerr << "unknown attribute of <node>: " << local_name << endl;
			return -2;
		}
	}
	if (node_id == 0) {
		cerr << "missing node-id of <node>" << endl;
		return -2;
	}
	std::unique_ptr<Node> node(new Node(node_id));
	i = xmlTextReaderRead(text_reader_);
	while (i > 0) {
		int type = xmlTextReaderNodeType(text_reader_);
		if (type == XML_READER_TYPE_ELEMENT) {
			const xmlChar *local_name = xmlTextReaderConstLocalName(text_reader_);
			if (xmlStrEqual(local_name, BAD_CAST "name")) {
				i = ReadNodeName(node.get());
				if (i <= 0) return i;
				continue;
			} else if (xmlStrEqual(local_name, BAD_CAST "description")) {
				// ignore description
				i = xmlTextReaderNext(text_reader_);
				continue;
			} else {
				cerr << "unknown child of <node>: " << local_name << endl;
				return -2;
			}
		} else if (type == XML_READER_TYPE_END_ELEMENT) {
			const xmlChar *local_name = xmlTextReaderConstLocalName(text_reader_);
			if (xmlStrEqual(local_name, BAD_CAST "node")) {
				if (!node->name()) {
					cerr << "missing <name> of <node>" << endl;
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
		cerr << "node name is empty" << endl;
		xmlFree(s);
		return -2;
	}
	if (ContainNonGraphic(name)) {
		cerr << "node name contains invalid character: \"" << name << "\"" << endl;
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
			if (xmlStrEqual(local_name, BAD_CAST "arc")) {
				i = ReadArc();
				if (i <= 0) return i;
				continue;
			} else {
				cerr << "unknown child of <arc-set>: " << local_name << endl;
				return -2;
			}
		} else if (type == XML_READER_TYPE_END_ELEMENT) {
			const xmlChar *local_name = xmlTextReaderConstLocalName(text_reader_);
			if (xmlStrEqual(local_name, BAD_CAST "arc-set")) {
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
		if (xmlStrEqual(local_name, BAD_CAST "arc-id")) {
			const xmlChar *value = xmlTextReaderConstValue(text_reader_);
			arc_id = atoi((const char *)value);
			if (arc_id <= 0) {
				cerr << "invalid value of arc-id of <arc>: " << value << endl;
				return -2;
			}
		} else if (xmlStrEqual(local_name, BAD_CAST "head-node-id")) {
			const xmlChar *value = xmlTextReaderConstValue(text_reader_);
			head_node_id = std::atoi(reinterpret_cast<const char *>(value));
			if (arc_id <= 0) {
				cerr << "invalid value of head-node-id of <arc>: " << value << endl;
				return -2;
			}
		} else if (xmlStrEqual(local_name, BAD_CAST "tail-node-id")) {
			const xmlChar *value = xmlTextReaderConstValue(text_reader_);
			tail_node_id = std::atoi(reinterpret_cast<const char *>(value));
			if (arc_id <= 0) {
				cerr << "invalid value of tail-node-id of <arc>: " << value << endl;
				return -2;
			}
		} else {
			cerr << "unknown attribute of <arc>: " << local_name << endl;
			return -2;
		}
	}
	if (arc_id == 0) {
		cerr << "missing arc-id of <arc>" << endl;
		return -2;
	}
	if (head_node_id == 0) {
		cerr << "missing head-node-id of <arc>" << endl;
		return -2;
	}
	if (tail_node_id == 0) {
		cerr << "missing tail-node-id of <arc>" << endl;
		return -2;
	}
	std::unique_ptr<Arc> arc(new Arc(arc_id, tail_node_id, head_node_id));
	i = xmlTextReaderRead(text_reader_);
	while (i > 0) {
		int type = xmlTextReaderNodeType(text_reader_);
		if (type == XML_READER_TYPE_ELEMENT) {
			const xmlChar *local_name = xmlTextReaderConstLocalName(text_reader_);
			if (xmlStrEqual(local_name, BAD_CAST "transition")) {
				i = ReadTransition(arc.get());
				if (i <= 0) return i;
				continue;
			} else {
				cerr << "unknown child of <arc>: " << local_name << endl;
				return -2;
			}
		} else if (type == XML_READER_TYPE_END_ELEMENT) {
			const xmlChar *local_name = xmlTextReaderConstLocalName(text_reader_);
			if (xmlStrEqual(local_name, BAD_CAST "arc")) {
				if (arc->type() == Arc::kUnspecified) {
					cerr << "missing transition of <arc>: " << arc_id << endl;
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
		if (xmlStrEqual(local_name, BAD_CAST "type")) {
			const xmlChar *value = xmlTextReaderConstValue(text_reader_);
			if (xmlStrEqual(value, BAD_CAST "condition")) {
				arc->set_type(Arc::kCondition);
			} else if (xmlStrEqual(value, BAD_CAST "probability")) {
				arc->set_type(Arc::kProbability);
			} else {
				cerr << "unknown type of <transition>: " << value << endl;
				return -2;
			}
		} else {
			cerr << "unknown attribute of <transition>: " << local_name << endl;
			return -2;
		}
	}
	if (arc->type() == Arc::kUnspecified) {
		cerr << "missing type of <transition>" << endl;
		return -2;
	}
	i = xmlTextReaderRead(text_reader_);
	while (i > 0) {
		int type = xmlTextReaderNodeType(text_reader_);
		if (type == XML_READER_TYPE_ELEMENT) {
			const xmlChar *local_name = xmlTextReaderConstLocalName(text_reader_);
			if (xmlStrEqual(local_name, BAD_CAST "math")) {
				mathml::MathDumper math_dumper(text_reader_, &arc->stream());
				i = math_dumper.Read(this);
				if (i <= 0) return i;
				continue;
			} else {
				cerr << "unknown child of <transition>: " << local_name << endl;
				return -2;
			}
		} else if (type == XML_READER_TYPE_END_ELEMENT) {
			const xmlChar *local_name = xmlTextReaderConstLocalName(text_reader_);
			if (xmlStrEqual(local_name, BAD_CAST "transition")) {
				return xmlTextReaderRead(text_reader_);
			}
		}
		i = xmlTextReaderRead(text_reader_);
	}
	return i;
}

}
}
