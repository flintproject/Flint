/* -*- Mode: C++; tab-width: 4; indent-tabs-mode: t; c-basic-offset: 4 -*- vim:set ts=4 sw=4 sts=4 noet: */
#ifndef FLINT_MATHML_MATH_DUMPER_H_
#define FLINT_MATHML_MATH_DUMPER_H_

#include <cctype>
#include <cstdio>
#include <cstdlib>
#include <cstring>
#include <iostream>
#include <libxml/xmlreader.h>

namespace mathml {

using std::cerr;
using std::endl;

class MathDumper {
public:
	MathDumper(xmlTextReaderPtr &text_reader, std::ostream *os)
		: text_reader_(text_reader),
		  name_(NULL),
		  found_(),
		  os_(os)
	{}

	MathDumper(xmlTextReaderPtr &text_reader, const xmlChar *name, std::ostream *os)
		: text_reader_(text_reader),
		  name_(name),
		  found_(),
		  os_(os)
	{}

	bool TargetIsFound() const {
		return found_;
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
				if (xmlStrEqual(local_name, BAD_CAST "math")) {
					return xmlTextReaderRead(text_reader_);
				}
			}
			i = xmlTextReaderRead(text_reader_);
		}
		return i;
	}

	int ReadElement(int space);

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
				if (xmlStrEqual(local_name, BAD_CAST "apply")) {
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
				if (xmlStrEqual(local_name, BAD_CAST "bvar")) {
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
			cerr << "missing body of <ci>" << endl;
			return -2;
		}
		xmlChar *name;
		int i = Trim(s, &name);
		if (i <= 0) return i;
		// check if it is the name
		if (name_ && !found_ && xmlStrEqual(name, name_)) {
			found_ = true;
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
			if (xmlStrEqual(local_name, BAD_CAST "type")) {
				const xmlChar *value = xmlTextReaderConstValue(text_reader_);
				if (xmlStrEqual(value, BAD_CAST "integer")) {
					cn_type = kInteger;
					break;
				} else if (xmlStrEqual(value, BAD_CAST "real")) {
					cn_type = kReal;
					break;
				} else if (xmlStrEqual(value, BAD_CAST "e-notation")) {
					cn_type = kENotation;
					break;
				} else if (xmlStrEqual(value, BAD_CAST "rational")) {
					cn_type = kRational;
					break;
				} else {
					cerr << "unsupported type of <cn>: " << value << endl;
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
					cerr << "missing body of <cn>" << endl;
					return -2;
				}
				xmlChar *t;
				i = Trim(s, &t);
				if (i <= 0) return i;
				*os_ << (const char *)t;
				xmlFree(s);
			} else if (type == XML_READER_TYPE_ELEMENT) {
				const xmlChar *local_name = xmlTextReaderConstLocalName(text_reader_);
				if (!xmlStrEqual(local_name, BAD_CAST "sep")) {
					cerr << "unexpected element in <cn>: " << local_name << endl;
					return -2;
				}
				if (cn_type == kENotation) {
					*os_ << "e";
				} else if (cn_type == kRational) {
					*os_ << "/";
				} else {
					cerr << "<sep> in <cn> of wrong type" << endl;
					return -2;
				}
			} else if (type == XML_READER_TYPE_END_ELEMENT) {
				const xmlChar *local_name = xmlTextReaderConstLocalName(text_reader_);
				if (xmlStrEqual(local_name, BAD_CAST "cn")) {
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
			cerr << "missing body of <csymbol>" << endl;
			return -2;
		}
		xmlChar *name;
		int i = Trim(s, &name);
		if (i <= 0) return i;
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
				if (xmlStrEqual(local_name, BAD_CAST "logbase")) {
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
				if (xmlStrEqual(local_name, BAD_CAST "matrix")) {
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
				if (xmlStrEqual(local_name, BAD_CAST "matrixrow")) {
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
				if (xmlStrEqual(local_name, BAD_CAST "otherwise")) {
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
				if (xmlStrEqual(local_name, BAD_CAST "piece")) {
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
				if (xmlStrEqual(local_name, BAD_CAST "piecewise")) {
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
				if (xmlStrEqual(local_name, BAD_CAST "vector")) {
					os_->put(')');
					return xmlTextReaderRead(text_reader_);
				}
			}
			i = xmlTextReaderRead(text_reader_);
		}
		return i;
	}

private:
	int Trim(xmlChar *s, xmlChar **tp) {
		int w = static_cast<int>(strlen((const char *)s));
		if (w == 0) {
			*tp = s;
			return 1;
		}
		do {
			int k = w;
			int c = xmlGetUTF8Char(s, &k);
			if (c < 0) {
				cerr << "invalid UTF-8 string: " << s << endl;
				return -2;
			}
			if (!std::isspace(c)) break;
			s += k;
			w -= k;
		} while (*s);

		*tp = s;

		w = static_cast<int>(strlen((const char *)s));
		if (w == 0) return 1;
		do {
			int k = w;
			int c = xmlGetUTF8Char(s, &k);
			if (c < 0) {
				cerr << "invalid UTF-8 string:" << s << endl;
				return -2;
			}
			if (std::isspace(c)) {
				*s = '\0';
				break;
			}
			s += k;
			w -= k;
		} while (*s);
		return 1;
	}

	xmlTextReaderPtr &text_reader_;
	const xmlChar *name_;
	bool found_;
	std::ostream *os_;
};

namespace {

struct ElementEntry {
	const char *local_name;
	int (MathDumper::*reader)();
} kElementTable[] = {
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

} // namespace mathml

#endif
