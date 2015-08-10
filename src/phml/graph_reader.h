/* -*- Mode: C++; tab-width: 4; indent-tabs-mode: t; c-basic-offset: 4 -*- vim:set ts=4 sw=4 sts=4 noet: */
#ifndef FLINT_PHML_GRAPH_READER_H_
#define FLINT_PHML_GRAPH_READER_H_

#include <cassert>
#include <cctype>
#include <memory>
#include <boost/noncopyable.hpp>
#include <libxml/xmlreader.h>

#include "mathml/math_dumper.h"

namespace flint {
namespace phml {

using std::cerr;
using std::endl;
using mathml::MathDumper;

class Node : boost::noncopyable {
public:
	explicit Node(int node_id)
		: node_id_(node_id),
		  name_(NULL)
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

class Arc {
public:
	enum Type {
		kUnspecified,
		kCondition,
		kProbability
	};

	explicit Arc(int arc_id)
		: arc_id_(arc_id),
		  tail_node_id_(),
		  head_node_id_(),
		  type_(kUnspecified)
	{}

	int arc_id() const {return arc_id_;}
	int tail_node_id() const {return tail_node_id_;}
	void set_tail_node_id(int tail_node_id) {tail_node_id_ = tail_node_id;}
	int head_node_id() const {return head_node_id_;}
	void set_head_node_id(int head_node_id) {head_node_id_ = head_node_id;}
	Type type() const {return type_;}
	void set_type(Type type) {type_ = type;}

	std::ostringstream &stream() {return stream_;}

	std::string GetMath() const {
		return stream_.str();
	}

private:
	int arc_id_;
	int tail_node_id_;
	int head_node_id_;
	Type type_;
	std::ostringstream stream_;
};

template<typename TPQ, typename TDriver>
class GraphReader {
public:
	GraphReader(const TPQ *pq, xmlTextReaderPtr &text_reader, TDriver *driver)
		: pq_(pq),
		  text_reader_(text_reader),
		  driver_(driver)
	{}

	int Read() {
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

	int Handle(int i) {
		return i;
	}

private:
	int ReadGraph()
	{
		int i = xmlTextReaderRead(text_reader_);
		while (i > 0) {
			int type = xmlTextReaderNodeType(text_reader_);
			if (type == XML_READER_TYPE_ELEMENT) {
				const xmlChar *local_name = xmlTextReaderConstLocalName(text_reader_);
				if (xmlStrEqual(local_name, BAD_CAST "node-set")) {
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

	int ReadNodeSet()
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

	int ReadNode()
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

	int ReadNodeName(Node *node)
	{
		xmlChar *s = xmlTextReaderReadString(text_reader_);
		assert(s);

		// validate name
		int len = xmlStrlen(s);
		for (int i=0;i<len;i++) {
			if (!isprint(s[i])) {
				cerr << "node name contains invalid character: \"" << s << "\"" << endl;
				xmlFree(s);
				return -2;
			}
		}

		node->set_name(s);
		return xmlTextReaderNext(text_reader_);
	}

	int ReadArcSet()
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

	int ReadArc()
	{
		int arc_id = 0;
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
			} else {
				cerr << "unknown attribute of <arc>: " << local_name << endl;
				return -2;
			}
		}
		if (arc_id == 0) {
			cerr << "missing arc-id of <arc>" << endl;
			return -2;
		}
		std::unique_ptr<Arc> arc(new Arc(arc_id));
		i = xmlTextReaderRead(text_reader_);
		while (i > 0) {
			int type = xmlTextReaderNodeType(text_reader_);
			if (type == XML_READER_TYPE_ELEMENT) {
				const xmlChar *local_name = xmlTextReaderConstLocalName(text_reader_);
				if (xmlStrEqual(local_name, BAD_CAST "tail")) {
					i = ReadTail(arc.get());
					if (i <= 0) return i;
					continue;
				} else if (xmlStrEqual(local_name, BAD_CAST "head")) {
					i = ReadHead(arc.get());
					if (i <= 0) return i;
					continue;
				} else if (xmlStrEqual(local_name, BAD_CAST "transition")) {
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
					if (arc->tail_node_id() == 0) {
						cerr << "missing <tail> of <arc>: " << arc_id << endl;
						return -2;
					}
					if (arc->head_node_id() == 0) {
						cerr << "missing <head> of <arc>: " << arc_id << endl;
						return -2;
					}
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

	int ReadTail(Arc *arc)
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
					cerr << "invalid value of node-id of <tail>: " << value << endl;
					return -2;
				}
			} else {
				cerr << "unknown attribute of <tail>: " << local_name << endl;
				return -2;
			}
		}
		if (node_id == 0) {
			cerr << "missing node-id of <tail>" << endl;
			return -2;
		}
		arc->set_tail_node_id(node_id);
		return xmlTextReaderNext(text_reader_);
	}

	int ReadHead(Arc *arc)
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
					cerr << "invalid value of node-id of <head>: " << value << endl;
					return -2;
				}
			} else {
				cerr << "unknown attribute of <head>: " << local_name << endl;
				return -2;
			}
		}
		if (node_id == 0) {
			cerr << "missing node-id of <head>" << endl;
			return -2;
		}
		arc->set_head_node_id(node_id);
		return xmlTextReaderNext(text_reader_);
	}

	int ReadTransition(Arc *arc)
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
					MathDumper math_dumper(text_reader_, &arc->stream());
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

	const TPQ *pq_;
	xmlTextReaderPtr &text_reader_;
	TDriver *driver_;
};

}
}

#endif
