/* -*- Mode: C++; tab-width: 4; indent-tabs-mode: t; c-basic-offset: 4 -*- vim:set ts=4 sw=4 sts=4 noet: */
#include "import.h"

#include <cassert>
#include <cstdio>
#include <cstdlib>
#include <cstring>
#include <iostream>
#include <memory>
#include <string>

#include <boost/uuid/uuid_io.hpp>
#include <libxml/parser.h>
#include <libxml/xpath.h>
#include <libxml/xpathInternals.h>

#include "modelpath.h"

namespace flint {
namespace {

class Parser {
public:
	Parser(const std::string &uuid, xmlDocPtr doc)
		: uuid_(uuid),
		  doc_(doc),
		  context_(nullptr),
		  object_(nullptr)
	{
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
			std::cerr << "failed to create XPath context" << std::endl;
			return false;
		}
		std::unique_ptr<char[]> pattern(new char[128]);
		std::sprintf(pattern.get(), "//is:module[@module-id='%s']/is:import/*", uuid_.c_str());
		xmlXPathRegisterNs(context_,
						   reinterpret_cast<const xmlChar *>("is"),
						   reinterpret_cast<const xmlChar *>("http://www.physiome.jp/ns/insilicoml"));
		object_ = xmlXPathEvalExpression(reinterpret_cast<const xmlChar *>(pattern.get()), context_);
		if (!object_) {
			std::cerr << "invalid XPath pattern: " << pattern.get() << std::endl;
			return false;
		}
		assert(object_->type == XPATH_NODESET);
		assert(object_->nodesetval);
		if (object_->nodesetval->nodeNr == 0) {
			std::cerr << "no such <import>" << std::endl;
			return false;
		}
		if (object_->nodesetval->nodeNr > 1) {
			std::cerr << "invalid <import>: " << object_->nodesetval->nodeNr << std::endl;
			return false;
		}
		xmlNodePtr node = xmlDocSetRootElement(doc_, object_->nodesetval->nodeTab[0]);
		if (node) xmlUnlinkNode(node);
		FILE *fp = std::fopen(dump_file, "w");
		if (!fp) {
			std::cerr << "failed to open dump file: " << dump_file << std::endl;
			return false;
		}
		xmlDocDump(fp, doc_);
		std::fclose(fp);
		if (node) xmlFreeNode(node);
		return true;
	}

private:
	std::string uuid_;
	xmlDocPtr doc_;
	xmlXPathContextPtr context_;
	xmlXPathObjectPtr object_;
};

} // namespace

bool DumpImport(sqlite3 *db, const boost::uuids::uuid &uuid)
{
	std::unique_ptr<char[]> model_file(GetModelFilename(db));
	if (!model_file)
		return false;
	xmlDocPtr doc = xmlParseFile(model_file.get());
	if (!doc) {
		std::cerr << "xml file seems malformed: " << model_file.get() << std::endl;
		return false;
	}
	std::unique_ptr<char[]> dump_file(new char[64]);
	std::string us = boost::uuids::to_string(uuid);
	std::sprintf(dump_file.get(), "%s.xml", us.c_str());
	std::unique_ptr<Parser> parser(new Parser(us, doc));
	return parser->Dump(dump_file.get());
}

}
