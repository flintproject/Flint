/* -*- Mode: C++; tab-width: 4; indent-tabs-mode: t; c-basic-offset: 4 -*- vim:set ts=4 sw=4 sts=4 noet: */
#ifndef FLINT_MATHML_MATH_DUMPER_H_
#define FLINT_MATHML_MATH_DUMPER_H_

#include <cctype>
#include <cstdio>
#include <cstdlib>
#include <cstring>
#include <iostream>
#include <libxml/xmlreader.h>

#include "flint/utf8string.h"

namespace flint {
namespace mathml {

class MathDumper {
public:
	MathDumper(xmlTextReaderPtr &text_reader, std::ostream *os)
		: text_reader_(text_reader),
		  name_(nullptr),
		  found_(false),
		  independent_(true),
		  os_(os)
	{}

	MathDumper(xmlTextReaderPtr &text_reader, const xmlChar *name, std::ostream *os)
		: text_reader_(text_reader),
		  name_(name),
		  found_(false),
		  independent_(true),
		  os_(os)
	{}

	bool TargetIsFound() const {
		return found_;
	}

	bool TargetIsIndependent() const {
		return independent_;
	}

	template<typename THandler>
	int Read(THandler *handler) {
		found_ = false;
		int i = xmlTextReaderRead(text_reader_);
		while (i > 0) {
			int type = xmlTextReaderNodeType(text_reader_);
			if (type == XML_READER_TYPE_ELEMENT) {
				i = ReadElement(1);
				if (i <= 0) return i;
				i = handler->Handle(i);
				if (i <= 0) return i;
				continue;
			} else if (type == XML_READER_TYPE_END_ELEMENT) {
				const xmlChar *local_name = xmlTextReaderConstLocalName(text_reader_);
				if (xmlStrEqual(local_name, reinterpret_cast<const xmlChar *>("math"))) {
					return xmlTextReaderRead(text_reader_);
				}
			}
			i = xmlTextReaderRead(text_reader_);
		}
		return i;
	}

	inline int ReadElement(int space);

	int ReadApply()
	{
		os_->put('(');
		int i = xmlTextReaderRead(text_reader_);
		int space = 0;
		while (i > 0) {
			int type = xmlTextReaderNodeType(text_reader_);
			if (type == XML_READER_TYPE_ELEMENT) {
				i = ReadElement(space++);
				if (i <= 0) return i;
				continue;
			} else if (type == XML_READER_TYPE_END_ELEMENT) {
				const xmlChar *local_name = xmlTextReaderConstLocalName(text_reader_);
				if (xmlStrEqual(local_name, reinterpret_cast<const xmlChar *>("apply"))) {
					os_->put(')');
					return xmlTextReaderRead(text_reader_);
				}
			}
			i = xmlTextReaderRead(text_reader_);
		}
		return i;
	}

	int ReadBvar()
	{
		*os_ << "(bvar";
		int i = xmlTextReaderRead(text_reader_);
		while (i > 0) {
			int type = xmlTextReaderNodeType(text_reader_);
			if (type == XML_READER_TYPE_ELEMENT) {
				i = ReadElement(1);
				if (i <= 0) return i;
				continue;
			} else if (type == XML_READER_TYPE_END_ELEMENT) {
				const xmlChar *local_name = xmlTextReaderConstLocalName(text_reader_);
				if (xmlStrEqual(local_name, reinterpret_cast<const xmlChar *>("bvar"))) {
					os_->put(')');
					return xmlTextReaderRead(text_reader_);
				}
			}
			i = xmlTextReaderRead(text_reader_);
		}
		return i;
	}

	int ReadCi()
	{
		xmlChar *s = xmlTextReaderReadString(text_reader_);
		if (!s) {
			std::cerr << "missing body of <ci>" << std::endl;
			return -2;
		}
		xmlChar *name;
		int i = Trim(s, &name);
		if (i == 0) return -2;
		if (name_) {
			// check whether it is the name or not
			if (!found_ && xmlStrEqual(name, name_))
				found_ = true;
			if (independent_ && !xmlStrEqual(name, name_))
				independent_ = false;
		}
		*os_ << "%" << (const char *)name;
		xmlFree(s);
		return xmlTextReaderRead(text_reader_);
	}

	int ReadCn()
	{
		enum CnType {
			kInteger,
			kReal,
			kENotation,
			kRational
		} cn_type = kReal;

		int i;
		while ( (i = xmlTextReaderMoveToNextAttribute(text_reader_)) > 0) {
			if (xmlTextReaderIsNamespaceDecl(text_reader_)) continue;

			const xmlChar *local_name = xmlTextReaderConstLocalName(text_reader_);
			if (xmlStrEqual(local_name, reinterpret_cast<const xmlChar *>("type"))) {
				const xmlChar *value = xmlTextReaderConstValue(text_reader_);
				if (xmlStrEqual(value, reinterpret_cast<const xmlChar *>("integer"))) {
					cn_type = kInteger;
					break;
				} else if (xmlStrEqual(value, reinterpret_cast<const xmlChar *>("real"))) {
					cn_type = kReal;
					break;
				} else if (xmlStrEqual(value, reinterpret_cast<const xmlChar *>("e-notation"))) {
					cn_type = kENotation;
					break;
				} else if (xmlStrEqual(value, reinterpret_cast<const xmlChar *>("rational"))) {
					cn_type = kRational;
					break;
				} else {
					std::cerr << "unsupported type of <cn>: " << value << std::endl;
					return -2;
				}
			}
		}

		i = xmlTextReaderRead(text_reader_);
		while (i > 0) {
			int type = xmlTextReaderNodeType(text_reader_);
			if (type == XML_READER_TYPE_TEXT) {
				xmlChar *s = xmlTextReaderValue(text_reader_);
				if (!s) {
					std::cerr << "missing body of <cn>" << std::endl;
					return -2;
				}
				xmlChar *t;
				i = Trim(s, &t);
				if (i == 0) return -2;
				*os_ << (const char *)t;
				xmlFree(s);
			} else if (type == XML_READER_TYPE_ELEMENT) {
				const xmlChar *local_name = xmlTextReaderConstLocalName(text_reader_);
				if (!xmlStrEqual(local_name, reinterpret_cast<const xmlChar *>("sep"))) {
					std::cerr << "unexpected element in <cn>: " << local_name << std::endl;
					return -2;
				}
				if (cn_type == kENotation) {
					*os_ << "e";
				} else if (cn_type == kRational) {
					*os_ << "/";
				} else {
					std::cerr << "<sep> in <cn> of wrong type" << std::endl;
					return -2;
				}
			} else if (type == XML_READER_TYPE_END_ELEMENT) {
				const xmlChar *local_name = xmlTextReaderConstLocalName(text_reader_);
				if (xmlStrEqual(local_name, reinterpret_cast<const xmlChar *>("cn"))) {
					return xmlTextReaderRead(text_reader_);
				}
			}
			i = xmlTextReaderRead(text_reader_);
		}
		return i;
	}

	int ReadCsymbol()
	{
		xmlChar *s = xmlTextReaderReadString(text_reader_);
		if (!s) {
			std::cerr << "missing body of <csymbol>" << std::endl;
			return -2;
		}
		xmlChar *name;
		int i = Trim(s, &name);
		if (i == 0) return -2;
		*os_ << "$" << (const char *)name;
		xmlFree(s);
		return xmlTextReaderRead(text_reader_);
	}

	int ReadLogbase()
	{
		*os_ << "(logbase";
		int i = xmlTextReaderRead(text_reader_);
		while (i > 0) {
			int type = xmlTextReaderNodeType(text_reader_);
			if (type == XML_READER_TYPE_ELEMENT) {
				i = ReadElement(1);
				if (i <= 0) return i;
				continue;
			} else if (type == XML_READER_TYPE_END_ELEMENT) {
				const xmlChar *local_name = xmlTextReaderConstLocalName(text_reader_);
				if (xmlStrEqual(local_name, reinterpret_cast<const xmlChar *>("logbase"))) {
					os_->put(')');
					return xmlTextReaderRead(text_reader_);
				}
			}
			i = xmlTextReaderRead(text_reader_);
		}
		return i;
	}

	int ReadMatrix()
	{
		*os_ << "(matrix";
		int i = xmlTextReaderRead(text_reader_);
		while (i > 0) {
			int type = xmlTextReaderNodeType(text_reader_);
			if (type == XML_READER_TYPE_ELEMENT) {
				i = ReadElement(1);
				if (i <= 0) return i;
				continue;
			} else if (type == XML_READER_TYPE_END_ELEMENT) {
				const xmlChar *local_name = xmlTextReaderConstLocalName(text_reader_);
				if (xmlStrEqual(local_name, reinterpret_cast<const xmlChar *>("matrix"))) {
					os_->put(')');
					return xmlTextReaderRead(text_reader_);
				}
			}
			i = xmlTextReaderRead(text_reader_);
		}
		return i;
	}

	int ReadMatrixrow()
	{
		*os_ << "(matrixrow";
		int i = xmlTextReaderRead(text_reader_);
		while (i > 0) {
			int type = xmlTextReaderNodeType(text_reader_);
			if (type == XML_READER_TYPE_ELEMENT) {
				i = ReadElement(1);
				if (i <= 0) return i;
				continue;
			} else if (type == XML_READER_TYPE_END_ELEMENT) {
				const xmlChar *local_name = xmlTextReaderConstLocalName(text_reader_);
				if (xmlStrEqual(local_name, reinterpret_cast<const xmlChar *>("matrixrow"))) {
					os_->put(')');
					return xmlTextReaderRead(text_reader_);
				}
			}
			i = xmlTextReaderRead(text_reader_);
		}
		return i;
	}

	int ReadOtherwise()
	{
		*os_ << "(otherwise";
		int i = xmlTextReaderRead(text_reader_);
		while (i > 0) {
			int type = xmlTextReaderNodeType(text_reader_);
			if (type == XML_READER_TYPE_ELEMENT) {
				i = ReadElement(1);
				if (i <= 0) return i;
				continue;
			} else if (type == XML_READER_TYPE_END_ELEMENT) {
				const xmlChar *local_name = xmlTextReaderConstLocalName(text_reader_);
				if (xmlStrEqual(local_name, reinterpret_cast<const xmlChar *>("otherwise"))) {
					os_->put(')');
					return xmlTextReaderRead(text_reader_);
				}
			}
			i = xmlTextReaderRead(text_reader_);
		}
		return i;
	}

	int ReadPiece()
	{
		*os_ << "(piece";
		int i = xmlTextReaderRead(text_reader_);
		while (i > 0) {
			int type = xmlTextReaderNodeType(text_reader_);
			if (type == XML_READER_TYPE_ELEMENT) {
				i = ReadElement(1);
				if (i <= 0) return i;
				continue;
			} else if (type == XML_READER_TYPE_END_ELEMENT) {
				const xmlChar *local_name = xmlTextReaderConstLocalName(text_reader_);
				if (xmlStrEqual(local_name, reinterpret_cast<const xmlChar *>("piece"))) {
					os_->put(')');
					return xmlTextReaderRead(text_reader_);
				}
			}
			i = xmlTextReaderRead(text_reader_);
		}
		return i;
	}

	int ReadPiecewise()
	{
		*os_ << "(piecewise";
		int i = xmlTextReaderRead(text_reader_);
		while (i > 0) {
			int type = xmlTextReaderNodeType(text_reader_);
			if (type == XML_READER_TYPE_ELEMENT) {
				i = ReadElement(1);
				if (i <= 0) return i;
				continue;
			} else if (type == XML_READER_TYPE_END_ELEMENT) {
				const xmlChar *local_name = xmlTextReaderConstLocalName(text_reader_);
				if (xmlStrEqual(local_name, reinterpret_cast<const xmlChar *>("piecewise"))) {
					os_->put(')');
					return xmlTextReaderRead(text_reader_);
				}
			}
			i = xmlTextReaderRead(text_reader_);
		}
		return i;
	}

	int ReadVector()
	{
		*os_ << "(vector";
		int i = xmlTextReaderRead(text_reader_);
		while (i > 0) {
			int type = xmlTextReaderNodeType(text_reader_);
			if (type == XML_READER_TYPE_ELEMENT) {
				i = ReadElement(1);
				if (i <= 0) return i;
				continue;
			} else if (type == XML_READER_TYPE_END_ELEMENT) {
				const xmlChar *local_name = xmlTextReaderConstLocalName(text_reader_);
				if (xmlStrEqual(local_name, reinterpret_cast<const xmlChar *>("vector"))) {
					os_->put(')');
					return xmlTextReaderRead(text_reader_);
				}
			}
			i = xmlTextReaderRead(text_reader_);
		}
		return i;
	}

private:
	xmlTextReaderPtr &text_reader_;
	const xmlChar *name_;
	bool found_;
	bool independent_;
	std::ostream *os_;
};

namespace {

struct ElementEntry {
	const char *local_name;
	int (MathDumper::*reader)();
};

const ElementEntry kElementTable[] = {
	// The following entries MUST be in bibliography order
	{"apply", &MathDumper::ReadApply},
	{"bvar", &MathDumper::ReadBvar},
	{"ci", &MathDumper::ReadCi},
	{"cn", &MathDumper::ReadCn},
	{"csymbol", &MathDumper::ReadCsymbol},
	{"logbase", &MathDumper::ReadLogbase},
	{"matrix", &MathDumper::ReadMatrix},
	{"matrixrow", &MathDumper::ReadMatrixrow},
	{"otherwise", &MathDumper::ReadOtherwise},
	{"piece", &MathDumper::ReadPiece},
	{"piecewise", &MathDumper::ReadPiecewise},
	{"vector", &MathDumper::ReadVector}
};

#define NUM_OF_ELEMENT_ENTRIES (sizeof(kElementTable)/sizeof(kElementTable[0]))

int CompareElementEntry(const void *x, const void *y)
{
	const ElementEntry *ex = (const ElementEntry *)x;
	const ElementEntry *ey = (const ElementEntry *)y;
	return std::strcmp(ex->local_name, ey->local_name);
}

} // namespace

int MathDumper::ReadElement(int space) {
	const xmlChar *local_name = xmlTextReaderConstLocalName(text_reader_);
	ElementEntry key, *result;
	key.local_name = (const char *)local_name;
	result = static_cast<ElementEntry *>(std::bsearch(&key, kElementTable, NUM_OF_ELEMENT_ENTRIES,
													  sizeof(ElementEntry), CompareElementEntry));
	if (space) os_->put(' ');
	if (result) return (this->*(result->reader))();
	*os_ << (const char *)local_name;
	return xmlTextReaderRead(text_reader_);
}

}
}

#endif
