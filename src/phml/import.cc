/* -*- Mode: C++; tab-width: 4; indent-tabs-mode: t; c-basic-offset: 4 -*- vim:set ts=4 sw=4 sts=4 noet: */
#include "import.h"

#include <cassert>
#include <cstdio>
#include <cstdlib>
#include <cstring>
#include <iostream>
#include <string>

#include <boost/scoped_array.hpp>
#include <boost/scoped_ptr.hpp>

#include <libxml/parser.h>
#include <libxml/xpath.h>
#include <libxml/xpathInternals.h>

#include "modelpath.h"

using std::cerr;
using std::endl;
using std::fclose;
using std::fopen;
using std::sprintf;
using std::string;

static const size_t kUuidSize = 36;

namespace {

class Parser {
public:
	Parser(const char *uuid, xmlDocPtr doc)
		: uuid_(uuid),
		  doc_(doc),
		  context_(NULL),
		  object_(NULL)
	{
		assert(std::strlen(uuid) == kUuidSize);
		assert(doc);
	}

	~Parser() {
		if (object_) xmlXPathFreeObject(object_);
		if (context_) xmlXPathFreeContext(context_);
		xmlFreeDoc(doc_);
	}

	bool Dump(const char *dump_file) {
		context_ = xmlXPathNewContext(doc_);
		if (!context_) {
			cerr << "failed to create XPath context" << endl;
			return false;
		}
		boost::scoped_array<char> pattern(new char[128]);
		sprintf(pattern.get(), "//is:module[@module-id='%s']/is:import/*", uuid_);
		xmlXPathRegisterNs(context_, BAD_CAST "is", BAD_CAST "http://www.physiome.jp/ns/insilicoml");
		object_ = xmlXPathEvalExpression(BAD_CAST pattern.get(), context_);
		if (!object_) {
			cerr << "invalid XPath pattern: " << pattern.get() << endl;
			return false;
		}
		assert(object_->type == XPATH_NODESET);
		assert(object_->nodesetval);
		if (object_->nodesetval->nodeNr == 0) {
			cerr << "no such <import>" << endl;
			return false;
		}
		if (object_->nodesetval->nodeNr > 1) {
			cerr << "invalid <import>: " << object_->nodesetval->nodeNr << endl;
			return false;
		}
		xmlNodePtr node = xmlDocSetRootElement(doc_, object_->nodesetval->nodeTab[0]);
		if (node) xmlUnlinkNode(node);
		FILE *fp = fopen(dump_file, "w");
		if (!fp) {
			cerr << "failed to open dump file: " << dump_file << endl;
			return false;
		}
		xmlDocDump(fp, doc_);
		fclose(fp);
		if (node) xmlFreeNode(node);
		return true;
	}

private:
	const char *uuid_;
	xmlDocPtr doc_;
	xmlXPathContextPtr context_;
	xmlXPathObjectPtr object_;
};

} // namespace

bool DumpImport(const char *db_file, const char *uuid)
{
	boost::scoped_array<char> model_file(GetModelFilename(db_file));
	xmlDocPtr doc = xmlParseFile(model_file.get());
	if (!doc) {
		cerr << "xml file seems malformed: " << model_file.get() << endl;
		return false;
	}
	boost::scoped_array<char> dump_file(new char[64]);
	sprintf(dump_file.get(), "%s.xml", uuid);
	boost::scoped_ptr<Parser> parser(new Parser(uuid, doc));
	return parser->Dump(dump_file.get());
}
