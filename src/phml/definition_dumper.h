/* -*- Mode: C++; tab-width: 4; indent-tabs-mode: t; c-basic-offset: 4 -*- vim:set ts=4 sw=4 sts=4 noet: */
#ifndef FLINT_PHML_DEFINITION_DUMPER_H_
#define FLINT_PHML_DEFINITION_DUMPER_H_

#include <cassert>
#include <cstdio>
#include <iostream>
#include <memory>
#include <libxml/xmlreader.h>

#include "mathml/math_dumper.h"

namespace flint {
namespace phml {

using std::cerr;
using std::endl;
using mathml::MathDumper;

template<typename TReader>
class DefinitionDumper {
public:
	DefinitionDumper(xmlTextReaderPtr &text_reader,
					 TReader *reader)
		: text_reader_(text_reader),
		  name_(nullptr),
		  math_dumper_(text_reader, reader->GetOutputStream()),
		  reader_(reader)
	{}

	DefinitionDumper(xmlTextReaderPtr &text_reader,
					 const xmlChar *name,
					 TReader *reader)
		: text_reader_(text_reader),
		  name_(name),
		  math_dumper_(text_reader, name, reader->GetOutputStream()),
		  reader_(reader)
	{}

	int Read(int level) {
		int i;
		while ( (i = xmlTextReaderMoveToNextAttribute(text_reader_)) > 0) {
			if (xmlTextReaderIsNamespaceDecl(text_reader_)) continue;

			const xmlChar *local_name = xmlTextReaderConstLocalName(text_reader_);
			if (xmlStrEqual(local_name, reinterpret_cast<const xmlChar *>("type"))) {
				const xmlChar *value = xmlTextReaderConstValue(text_reader_);
				if (xmlStrEqual(value, reinterpret_cast<const xmlChar *>("assign"))) {
					// skip one of type assign
					return xmlTextReaderNext(text_reader_);
				} else if (xmlStrEqual(value, reinterpret_cast<const xmlChar *>("graph"))) {
					assert(false);
				} else if (xmlStrEqual(value, reinterpret_cast<const xmlChar *>("loop"))) {
					// skip one of type loop
					return xmlTextReaderNext(text_reader_);
				}
			}
		}

		reader_->OpenDefinition(level);
		i = xmlTextReaderRead(text_reader_);
		while (i > 0) {
			int type = xmlTextReaderNodeType(text_reader_);
			if (type == XML_READER_TYPE_ELEMENT) {
				const xmlChar *local_name = xmlTextReaderConstLocalName(text_reader_);
				if (xmlStrEqual(local_name, reinterpret_cast<const xmlChar *>("case-set"))) {
					i = ReadCaseSet(level);
					if (i <= 0) return i;
					continue;
				} else if (xmlStrEqual(local_name, reinterpret_cast<const xmlChar *>("math"))) {
					std::unique_ptr<Handler> handler(new Handler);
					i = math_dumper_.Read(handler.get());
					if (i < 0) return i;
					if (name_ && !math_dumper_.TargetIsFound()) {
						cerr << "invalid definition of <"
							 << TReader::kName
							 << '>'
							 << endl;
						return -2;
					}
					continue;
				} else {
					cerr << "unexpected child element of <definition>: " << local_name << endl;
					return -2;
				}
			} else if (type == XML_READER_TYPE_END_ELEMENT) {
				const xmlChar *local_name = xmlTextReaderConstLocalName(text_reader_);
				if (xmlStrEqual(local_name, reinterpret_cast<const xmlChar *>("definition"))) {
					reader_->CloseDefinition(level);
					return xmlTextReaderRead(text_reader_);
				}
			}
			i = xmlTextReaderRead(text_reader_);
		}
		return i;
	}

private:
	class Handler {
	public:
		Handler() : count_() {}

		int Handle(int i) {
			if (count_++) {
				cerr << "two or more elements in <math>" << endl;
				return -2;
			}
			return i;
		}

	private:
		int count_;
	};

	int ReadCaseSet(int level) {
		reader_->Print(" (case-set");
		int i = xmlTextReaderRead(text_reader_);
		while (i > 0) {
			int type = xmlTextReaderNodeType(text_reader_);
			if (type == XML_READER_TYPE_ELEMENT) {
				const xmlChar *local_name = xmlTextReaderConstLocalName(text_reader_);
				if (xmlStrEqual(local_name, reinterpret_cast<const xmlChar *>("case"))) {
					i = ReadCase(level);
					if (i <= 0) return i;
					continue;
				} else {
					cerr << "unknown child element of <case-set>: " << local_name << endl;
					return -2;
				}
			} else if (type == XML_READER_TYPE_END_ELEMENT) {
				const xmlChar *local_name = xmlTextReaderConstLocalName(text_reader_);
				if (xmlStrEqual(local_name, reinterpret_cast<const xmlChar *>("case-set"))) {
					reader_->Print(")");
					return xmlTextReaderRead(text_reader_);
				}
			}
			i = xmlTextReaderRead(text_reader_);
		}
		return i;
	}

	int ReadCase(int level) {
		reader_->Print(" (case");
		int i = xmlTextReaderRead(text_reader_);
		while (i > 0) {
			int type = xmlTextReaderNodeType(text_reader_);
			if (type == XML_READER_TYPE_ELEMENT) {
				const xmlChar *local_name = xmlTextReaderConstLocalName(text_reader_);
				if (xmlStrEqual(local_name, reinterpret_cast<const xmlChar *>("condition"))) {
					i = ReadCondition();
					if (i <= 0) return i;
					continue;
				} else if (xmlStrEqual(local_name, reinterpret_cast<const xmlChar *>("definition"))) {
					i = Read(level + 1);
					if (i <= 0) return i;
					continue;
				}
			} else if (type == XML_READER_TYPE_END_ELEMENT) {
				const xmlChar *local_name = xmlTextReaderConstLocalName(text_reader_);
				if (xmlStrEqual(local_name, reinterpret_cast<const xmlChar *>("case"))) {
					reader_->Print(")");
					return xmlTextReaderRead(text_reader_);
				}
			}
			i = xmlTextReaderRead(text_reader_);
		}
		return i;
	}

	int ReadCondition() {
		int i;
		while ( (i = xmlTextReaderMoveToNextAttribute(text_reader_)) > 0) {
			if (xmlTextReaderIsNamespaceDecl(text_reader_)) continue;

			const xmlChar *local_name = xmlTextReaderConstLocalName(text_reader_);
			if (xmlStrEqual(local_name, reinterpret_cast<const xmlChar *>("format"))) {
				const xmlChar *value = xmlTextReaderConstValue(text_reader_);
				if (!xmlStrEqual(value, reinterpret_cast<const xmlChar *>("mathml"))) {
					cerr << "unknown format of <condition>: " << value << endl;
					return -2;
				}
			}
		}
		reader_->Print(" (condition");
		i = xmlTextReaderRead(text_reader_);
		while (i > 0) {
			int type = xmlTextReaderNodeType(text_reader_);
			if (type == XML_READER_TYPE_ELEMENT) {
				const xmlChar *local_name = xmlTextReaderConstLocalName(text_reader_);
				if (xmlStrEqual(local_name, reinterpret_cast<const xmlChar *>("math"))) {
					std::unique_ptr<Handler> handler(new Handler);
					i = math_dumper_.Read(handler.get());
					if (i <= 0) return i;
					continue;
				} else {
					cerr << "unexpected child element of <condition>: " << local_name << endl;
					return -2;
				}
			} else if (type == XML_READER_TYPE_END_ELEMENT) {
				const xmlChar *local_name = xmlTextReaderConstLocalName(text_reader_);
				if (xmlStrEqual(local_name, reinterpret_cast<const xmlChar *>("condition"))) {
					reader_->Print(")");
					return xmlTextReaderRead(text_reader_);
				}
			}
			i = xmlTextReaderRead(text_reader_);
		}
		return i;
	}

	xmlTextReaderPtr &text_reader_;
	const xmlChar *name_;
	MathDumper math_dumper_;
	TReader *reader_;
};

}
}

#endif
