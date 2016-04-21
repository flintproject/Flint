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

using std::cerr;
using std::endl;
using std::fclose;
using std::fopen;
using std::fread;
using std::perror;
using std::strncmp;

namespace flint {
namespace file {

namespace {

/*
 * Return 1 if it is PHZ; 0 if it is not; -1 in case of error.
 */
int CheckPhz(const char *filename)
{
	assert(filename);

	FILE *fp = fopen(filename, "rb");
	if (!fp) {
		perror(filename);
		return -1;
	}
	char magic[2];
	int r = fread(magic, 2, 1, fp);
	fclose(fp);
	if (r != 1) {
		cerr << "failed to read magic in model file: "
			 << filename
			 << endl;
		return -1;
	}
	if (strncmp(magic, "PK", 2) == 0) { // looks like zip
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
				if ( xmlStrEqual(uri, BAD_CAST "http://www.physiodesigner.org/2013/ns/phsp/1.0") &&
					 xmlStrEqual(local_name, BAD_CAST "phsp") ) {
					*format_ = kPhsp;
					return true;
				} else if ( xmlStrEqual(uri, BAD_CAST "http://www.physiodesigner.org/2012/ns/physiodesigner/1.0") &&
					 xmlStrEqual(local_name, BAD_CAST "insilico-model") ) {
					*format_ = kPhml;
					return true;
				} else if ( xmlStrEqual(uri, BAD_CAST "http://www.physiome.jp/ns/insilicoml") &&
							xmlStrEqual(local_name, BAD_CAST "insilico-model") ) {
					*format_ = kIsml;
					return true;
				} else if ( xmlStrstr(uri, BAD_CAST "http://www.cellml.org/cellml/") &&
							xmlStrEqual(local_name, BAD_CAST "model") ) {
					*format_ = kCellml;
					return true;
				} else if ( xmlStrstr(uri, BAD_CAST "http://www.sbml.org/sbml/") &&
							xmlStrEqual(local_name, BAD_CAST "sbml") ) {
					*format_ = kSbml;
					return true;
				} else if ( xmlStrEqual(uri ,BAD_CAST "http://www.w3.org/1998/Math/MathML") &&
							xmlStrEqual(local_name, BAD_CAST "math")) {
					*format_ = kMathml;
					return true;
				} else if ( xmlStrEqual(uri ,BAD_CAST "http://sed-ml.org/") &&
							xmlStrEqual(local_name, BAD_CAST "sedML")) {
					*format_ = kSedml;
					return true;
				} else {
					cerr << "unknown pair of namespace and element";
					if (uri) cerr << ":" << uri;
					if (local_name) cerr << ": <" << local_name << ">";
					cerr << endl;
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
		cerr << "could not read the input: " << filename << endl;
		return false;
	}
	Reader reader(text_reader, format);
	return reader.Read();
}

}
}
