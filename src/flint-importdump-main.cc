/* -*- Mode: C++; tab-width: 4; indent-tabs-mode: t; c-basic-offset: 4 -*- vim:set ts=4 sw=4 sts=4 noet: */
#include <cassert>
#include <cstdio>
#include <cstdlib>
#include <iostream>
#include <string>

#include <boost/program_options.hpp>
#include <boost/scoped_array.hpp>
#include <boost/scoped_ptr.hpp>

#include <libxml/parser.h>
#include <libxml/xpath.h>
#include <libxml/xpathInternals.h>

#include "modelpath.h"

namespace po = boost::program_options;

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
	Parser(const string &uuid, xmlDocPtr doc)
		: uuid_(uuid),
		  doc_(doc),
		  context_(NULL),
		  object_(NULL)
	{
		assert(uuid.size() == kUuidSize);
		assert(doc);
	}

	~Parser() {
		if (object_) xmlXPathFreeObject(object_);
		if (context_) xmlXPathFreeContext(context_);
		xmlFreeDoc(doc_);
		xmlCleanupParser();
	}

	bool Dump(const char *dump_file) {
		context_ = xmlXPathNewContext(doc_);
		if (!context_) {
			cerr << "failed to create XPath context" << endl;
			return false;
		}
		boost::scoped_array<char> pattern(new char[128]);
		sprintf(pattern.get(), "//is:module[@module-id='%s']/is:import/*", uuid_.c_str());
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
	string uuid_;
	xmlDocPtr doc_;
	xmlXPathContextPtr context_;
	xmlXPathObjectPtr object_;
};

} // namespace

int main(int argc, char *argv[])
{
	LIBXML_TEST_VERSION
	xmlInitParser();

	po::options_description opts("options");
	po::positional_options_description popts;
	po::variables_map vm;
	string uuid, input_file;
	int print_help = 0;

	opts.add_options()
		("uuid", po::value<string>(&uuid), "Input uuid")
		("input", po::value<string>(&input_file), "Input file name")
		("help,h", "Show this message");
	popts.add("uuid", 1).add("input", 1);

	try {
		po::store(po::command_line_parser(argc, argv).options(opts).positional(popts).run(), vm);
		po::notify(vm);
		if (vm.count("help") > 0) print_help = 1;
		if (vm.count("uuid") == 0 || vm.count("input") == 0) print_help = 2;
	} catch (const po::error &) {
		print_help = 2;
	}
	if (print_help) {
		cerr << "usage: " << argv[0] << " UUID PATH" << endl;
		cerr << opts;
		return (print_help == 1) ? EXIT_SUCCESS : EXIT_FAILURE;
	}

	if (uuid.size() != kUuidSize) {
		cerr << "invalid UUID: " << uuid << endl;
		return EXIT_FAILURE;
	}

	boost::scoped_array<char> model_file(GetModelFilename(input_file.c_str()));
	xmlDocPtr doc = xmlParseFile(model_file.get());
	if (!doc) {
		cerr << "xml file seems malformed: " << model_file.get() << endl;
		return EXIT_FAILURE;
	}
	boost::scoped_array<char> dump_file(new char[64]);
	sprintf(dump_file.get(), "%s.xml", uuid.c_str());
	boost::scoped_ptr<Parser> parser(new Parser(uuid, doc));
	if (!parser->Dump(dump_file.get())) return EXIT_FAILURE;
	return EXIT_SUCCESS;
}
