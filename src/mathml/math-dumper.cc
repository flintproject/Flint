/* -*- Mode: C++; tab-width: 4; indent-tabs-mode: t; c-basic-offset: 4 -*- vim:set ts=4 sw=4 sts=4 noet: */
#include "mathml/math-dumper.h"

#include <cctype>
#include <cstdio>
#include <cstdlib>
#include <cstring>
#include <iostream>
#include <libxml/xmlreader.h>

#include "flint/utf8string.h"

namespace flint {
namespace mathml {

MathDumper::MathDumper(xmlTextReaderPtr &text_reader, std::ostream *os)
	: text_reader_(text_reader),
	  name_(nullptr),
	  found_(false),
	  independent_(true),
	  os_(os)
{}

MathDumper::MathDumper(xmlTextReaderPtr &text_reader, const xmlChar *name, std::ostream *os)
	: text_reader_(text_reader),
	  name_(name),
	  found_(false),
	  independent_(true),
	  os_(os)
{}

bool MathDumper::TargetIsFound() const
{
	return found_;
}

bool MathDumper::TargetIsIndependent() const
{
	return independent_;
}

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

}

int MathDumper::ReadElement(int space) {
	const xmlChar *local_name = xmlTextReaderConstLocalName(text_reader_);
	ElementEntry key, *result;
	key.local_name = reinterpret_cast<const char *>(local_name);
	result = static_cast<ElementEntry *>(std::bsearch(&key, kElementTable, NUM_OF_ELEMENT_ENTRIES,
													  sizeof(ElementEntry), CompareElementEntry));
	if (space) os_->put(' ');
	if (result) return (this->*(result->reader))();
	*os_ << reinterpret_cast<const char *>(local_name);
	return xmlTextReaderRead(text_reader_);
}

int MathDumper::ReadApply()
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

int MathDumper::ReadBvar()
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

int MathDumper::ReadCi()
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
	*os_ << "%" << reinterpret_cast<const char *>(name);
	xmlFree(s);
	return xmlTextReaderRead(text_reader_);
}

int MathDumper::ReadCn()
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
			*os_ << reinterpret_cast<const char *>(t);
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

int MathDumper::ReadCsymbol()
{
	xmlChar *s = xmlTextReaderReadString(text_reader_);
	if (!s) {
		std::cerr << "missing body of <csymbol>" << std::endl;
		return -2;
	}
	xmlChar *name;
	int i = Trim(s, &name);
	if (i == 0) return -2;
	*os_ << "$" << reinterpret_cast<const char *>(name);
	xmlFree(s);
	return xmlTextReaderRead(text_reader_);
}

int MathDumper::ReadLogbase()
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

int MathDumper::ReadMatrix()
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

int MathDumper::ReadMatrixrow()
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

int MathDumper::ReadOtherwise()
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

int MathDumper::ReadPiece()
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

int MathDumper::ReadPiecewise()
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

int MathDumper::ReadVector()
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

}
}
