/* -*- Mode: C++; tab-width: 4; indent-tabs-mode: t; c-basic-offset: 4 -*- vim:set ts=4 sw=4 sts=4 noet: */
#ifndef FLINT_PHML_GRAPH_READER_H_
#define FLINT_PHML_GRAPH_READER_H_

#include <libxml/xmlreader.h>

namespace flint {
namespace phml {

class Arc;
class DatabaseDriver;
class Node;
class PQ;

class GraphReader {
public:
	GraphReader(const GraphReader &) = delete;
	GraphReader &operator=(const GraphReader &) = delete;

	GraphReader(const PQ *pq, xmlTextReaderPtr &text_reader,
				DatabaseDriver *driver);

	int Read();

	int Handle(int i);

private:
	int ReadGraph();

	int ReadNodeSet();

	int ReadNode();

	int ReadNodeName(Node *node);

	int ReadArcSet();

	int ReadArc();

	int ReadTransition(Arc *arc);

	const PQ *pq_;
	xmlTextReaderPtr &text_reader_;
	DatabaseDriver *driver_;
};

}
}

#endif
