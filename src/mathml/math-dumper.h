/* -*- Mode: C++; tab-width: 4; indent-tabs-mode: t; c-basic-offset: 4 -*- vim:set ts=4 sw=4 sts=4 noet: */
#ifndef FLINT_MATHML_MATH_DUMPER_H_
#define FLINT_MATHML_MATH_DUMPER_H_

#include <iostream>
#include <libxml/xmlreader.h>

namespace flint {
namespace mathml {

class MathDumper {
public:
	MathDumper(xmlTextReaderPtr &text_reader, std::ostream *os);

	MathDumper(xmlTextReaderPtr &text_reader, const xmlChar *name, std::ostream *os);

	bool TargetIsFound() const;

	bool TargetIsIndependent() const;

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

	int ReadApply();
	int ReadBvar();
	int ReadCi();
	int ReadCn();
	int ReadCsymbol();
	int ReadLogbase();
	int ReadMatrix();
	int ReadMatrixrow();
	int ReadOtherwise();
	int ReadPiece();
	int ReadPiecewise();
	int ReadVector();

private:
	int ReadElement(int space);

	xmlTextReaderPtr &text_reader_;
	const xmlChar *name_;
	bool found_;
	bool independent_;
	std::ostream *os_;
};

}
}

#endif
