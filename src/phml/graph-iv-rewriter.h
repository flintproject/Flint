/* -*- Mode: C++; tab-width: 4; indent-tabs-mode: t; c-basic-offset: 4 -*- vim:set ts=4 sw=4 sts=4 noet: */
#ifndef FLINT_PHML_GRAPH_IV_REWRITER_H_
#define FLINT_PHML_GRAPH_IV_REWRITER_H_

#include <map>
#include <string>
#include <boost/uuid/uuid.hpp>

#include "sqlite3.h"

namespace flint {
namespace phml {

class GraphIvRewriter {
public:
	GraphIvRewriter(const GraphIvRewriter &) = delete;
	GraphIvRewriter &operator=(const GraphIvRewriter &) = delete;

	explicit GraphIvRewriter(sqlite3 *db);

	~GraphIvRewriter();

	bool Rewrite();

private:
	bool Process(sqlite3_int64 pq_rowid,
				 const boost::uuids::uuid &module_id,
				 const char *name,
				 const char *math);

	bool FindNode(sqlite3_int64 pq_rowid, const char *node_name, int *node_id);

	bool Update(sqlite3_int64 pq_rowid, const char *math);

	typedef std::map<sqlite3_int64, std::string> Map;

	sqlite3_stmt *stmt_graph_;
	sqlite3_stmt *stmt_node_;
	sqlite3_stmt *stmt_update_;
	Map m_;
};

}
}

#endif
