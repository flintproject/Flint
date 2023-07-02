/* -*- Mode: C++; tab-width: 4; indent-tabs-mode: t; c-basic-offset: 4 -*- vim:set ts=4 sw=4 sts=4 noet: */
#ifdef HAVE_CONFIG_H
#include "config.h"
#endif

#include "file.h"

#include <cassert>
#include <cstdio>
#include <cstdlib>
#include <cstring>
#include <iostream>

#include <libxml/xmlreader.h>

namespace flint {
namespace file {

namespace {

/*
 * Return 1 if it is PHZ; 0 if it is not; -1 in case of error.
 */
int CheckPhz(const char *filename)
{
	assert(filename);

	FILE *fp = std::fopen(filename, "rb");
	if (!fp) {
		std::perror(filename);
		return -1;
	}
	char magic[2];
	int r = std::fread(magic, 2, 1, fp);
	std::fclose(fp);
	if (r != 1) {
		std::cerr << "failed to read magic in model file: "
			 << filename
			 << std::endl;
		return -1;
	}
	if (std::strncmp(magic, "PK", 2) == 0) { // looks like zip
		return 1;
	}
	return 0;
}

class Reader {
public:
	Reader(xmlTextReaderPtr &text_reader, Format *format)
		: text_reader_(text_reader)
		, format_(format)
	{}

	~Reader() {
		xmlFreeTextReader(text_reader_);
	}

	bool Read() {
		for (int i=xmlTextReaderRead(text_reader_);i>0;i=xmlTextReaderRead(text_reader_)) {
			int type = xmlTextReaderNodeType(text_reader_);
			if (type == XML_READER_TYPE_ELEMENT) {
				const xmlChar *uri = xmlTextReaderConstNamespaceUri(text_reader_);
				const xmlChar *local_name = xmlTextReaderConstLocalName(text_reader_);
				if ( xmlStrEqual(uri, reinterpret_cast<const xmlChar *>("http://www.physiodesigner.org/2013/ns/phsp/1.0")) &&
					 xmlStrEqual(local_name, reinterpret_cast<const xmlChar *>("phsp")) ) {
					*format_ = kPhsp;
					return true;
				} else if ( xmlStrEqual(uri, reinterpret_cast<const xmlChar *>("http://www.physiodesigner.org/2012/ns/physiodesigner/1.0")) &&
					 xmlStrEqual(local_name, reinterpret_cast<const xmlChar *>("insilico-model")) ) {
					*format_ = kPhml;
					return true;
				} else if ( xmlStrEqual(uri, reinterpret_cast<const xmlChar *>("http://www.physiome.jp/ns/insilicoml")) &&
							xmlStrEqual(local_name, reinterpret_cast<const xmlChar *>("insilico-model")) ) {
					*format_ = kIsml;
					return true;
				} else if ( xmlStrstr(uri, reinterpret_cast<const xmlChar *>("http://www.cellml.org/cellml/")) &&
							xmlStrEqual(local_name, reinterpret_cast<const xmlChar *>("model")) ) {
					*format_ = kCellml;
					return true;
				} else if ( xmlStrstr(uri, reinterpret_cast<const xmlChar *>("http://www.sbml.org/sbml/")) &&
							xmlStrEqual(local_name, reinterpret_cast<const xmlChar *>("sbml")) ) {
					*format_ = kSbml;
					return true;
				} else if ( xmlStrEqual(uri ,reinterpret_cast<const xmlChar *>("http://www.w3.org/1998/Math/MathML")) &&
							xmlStrEqual(local_name, reinterpret_cast<const xmlChar *>("math"))) {
					*format_ = kMathml;
					return true;
				} else if ( xmlStrEqual(uri ,reinterpret_cast<const xmlChar *>("http://sed-ml.org/")) &&
							xmlStrEqual(local_name, reinterpret_cast<const xmlChar *>("sedML"))) {
					*format_ = kSedml;
					return true;
				} else {
					std::cerr << "unknown pair of namespace and element";
					if (uri) std::cerr << ":" << uri;
					if (local_name) std::cerr << ": <" << local_name << ">";
					std::cerr << std::endl;
					return false;
				}
			}
		}
		return false;
	}

private:
	xmlTextReaderPtr &text_reader_;
	Format *format_;
};

}

bool DetectFormat(const char *filename, Format *format)
{
	switch (CheckPhz(filename)) {
	case 0:
		// go to next
		break;
	case 1:
		*format = kPhz;
		return true;
	default:
		return false;
	}

	xmlTextReaderPtr text_reader = xmlReaderForFile(filename, nullptr, 0);
	if (!text_reader) {
		std::cerr << "could not read the input: " << filename << std::endl;
		return false;
	}
	Reader reader(text_reader, format);
	return reader.Read();
}

}
}
