/* -*- Mode: C++; tab-width: 4; indent-tabs-mode: t; c-basic-offset: 4 -*- vim:set ts=4 sw=4 sts=4 noet: */
#ifdef HAVE_CONFIG_H
#include "config.h"
#endif

#include <cassert>
#include <cstdio>
#include <cstdlib>
#include <iostream>
#include <string>

#include <boost/program_options.hpp>
#include <boost/scoped_array.hpp>
#include <boost/scoped_ptr.hpp>

#include <libxml/xmlreader.h>

#include "modelpath.h"

namespace po = boost::program_options;

using std::cerr;
using std::cout;
using std::endl;
using std::exit;
using std::fclose;
using std::fopen;
using std::fread;
using std::perror;
using std::string;
using std::strncmp;

namespace {

void CheckPhz(const char *model_file)
{
	assert(model_file);

	FILE *fp = fopen(model_file, "rb");
	if (!fp) {
		perror(model_file);
		exit(EXIT_FAILURE);
	}
	char magic[2];
	int r = fread(magic, 2, 1, fp);
	fclose(fp);
	if (r != 1) {
		cerr << "failed to read magic in model file: "
			 << model_file
			 << endl;
		exit(EXIT_FAILURE);
	}
	if (strncmp(magic, "PK", 2) == 0) { // looks like zip
		cout << "phz" << endl;
		exit(EXIT_SUCCESS);
	}
}

class Reader {
public:
	explicit Reader(xmlTextReaderPtr &text_reader) : text_reader_(text_reader) {}

	~Reader() {
		xmlFreeTextReader(text_reader_);
		xmlCleanupParser();
	}

	bool Read() {
		for (int i=xmlTextReaderRead(text_reader_);i>0;i=xmlTextReaderRead(text_reader_)) {
			int type = xmlTextReaderNodeType(text_reader_);
			if (type == XML_READER_TYPE_ELEMENT) {
				const xmlChar *uri = xmlTextReaderConstNamespaceUri(text_reader_);
				const xmlChar *local_name = xmlTextReaderConstLocalName(text_reader_);
				if ( xmlStrEqual(uri, BAD_CAST "http://www.physiodesigner.org/2013/ns/phsp/1.0") &&
					 xmlStrEqual(local_name, BAD_CAST "phsp") ) {
					cout << "phsp" << endl;
					return true;
				} else if ( xmlStrEqual(uri, BAD_CAST "http://www.physiodesigner.org/2012/ns/physiodesigner/1.0") &&
					 xmlStrEqual(local_name, BAD_CAST "insilico-model") ) {
					cout << "phml" << endl;
					return true;
				} else if ( xmlStrEqual(uri, BAD_CAST "http://www.physiome.jp/ns/insilicoml") &&
							xmlStrEqual(local_name, BAD_CAST "insilico-model") ) {
					cout << "isml" << endl;
					return true;
				} else if ( xmlStrstr(uri, BAD_CAST "http://www.cellml.org/cellml/") &&
							xmlStrEqual(local_name, BAD_CAST "model") ) {
					cout << "cellml" << endl;
					return true;
				} else if ( xmlStrstr(uri, BAD_CAST "http://www.sbml.org/sbml/") &&
							xmlStrEqual(local_name, BAD_CAST "sbml") ) {
					cout << "sbml" << endl;
					return true;
				} else if ( xmlStrEqual(uri ,BAD_CAST "http://www.w3.org/1998/Math/MathML") &&
							xmlStrEqual(local_name, BAD_CAST "math")) {
					cout << "mathml" << endl;
					return true;
				} else if ( xmlStrEqual(uri ,BAD_CAST "http://sed-ml.org/") &&
							xmlStrEqual(local_name, BAD_CAST "sedML")) {
					cout << "sedml" << endl;
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
};

} // namespace

int main(int argc, char *argv[])
{
	LIBXML_TEST_VERSION
	xmlInitParser();

	po::options_description opts("options");
	po::positional_options_description popts;
	po::variables_map vm;
	string input_file;
	int print_help = 0;

	opts.add_options()
		("input", po::value<string>(&input_file), "Input file name")
		("help,h", "Show this message");
	popts.add("input", 1);

	try {
		po::store(po::command_line_parser(argc, argv).options(opts).positional(popts).run(), vm);
		po::notify(vm);
		if (vm.count("help")) print_help = 1;
		else if (vm.count("input") == 0) print_help = 2;
	} catch (const po::error &) {
		print_help = 2;
	}
	if (print_help) {
		cerr << "usage: " << argv[0] << " PATH" << endl;
		cerr << opts;
		return (print_help == 1) ? EXIT_SUCCESS : EXIT_FAILURE;
	}

	boost::scoped_array<char> model_file(GetModelFilename(input_file.c_str()));

	CheckPhz(model_file.get());

	xmlTextReaderPtr text_reader = xmlReaderForFile(model_file.get(), NULL, 0);
	if (!text_reader) {
		cerr << "could not read the input" << endl;
		xmlCleanupParser();
		return EXIT_FAILURE;
	}

	boost::scoped_ptr<Reader> reader(new Reader(text_reader));
	if (!reader->Read()) return EXIT_FAILURE;

	return EXIT_SUCCESS;
}
