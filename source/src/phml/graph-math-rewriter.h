/* -*- Mode: C++; tab-width: 4; indent-tabs-mode: t; c-basic-offset: 4 -*- vim:set ts=4 sw=4 sts=4 noet: */
#ifndef FLINT_PHML_GRAPH_MATH_REWRITER_H_
#define FLINT_PHML_GRAPH_MATH_REWRITER_H_

#include <cstdio>
#include <iostream>
#include "db/utility.h"
#include "sqlite3.h"

namespace flint {
namespace phml {

class GraphMathRewriter {
public:
	GraphMathRewriter(const GraphMathRewriter &) = delete;
	GraphMathRewriter &operator=(const GraphMathRewriter &) = delete;

	template<int N_select, int N_update>
	GraphMathRewriter(const char (&query_select)[N_select],
					  const char (&query_update)[N_update],
					  sqlite3 *db)
		: stmt_select_(nullptr)
		, stmt_node_(nullptr)
		, stmt_update_(nullptr)
	{
		static const char kQueryNode[] = \
			"SELECT n.node_id FROM nodes AS n"
			" LEFT JOIN pqs AS p ON n.pq_rowid = p.rowid"
			" LEFT JOIN modules AS m ON p.module_rowid = m.rowid"
			" WHERE n.name = ?"
			" AND EXISTS (SELECT * FROM pqs WHERE rowid = ? AND module_rowid = m.rowid)";

		int e = sqlite3_prepare_v2(db, query_select, N_select, &stmt_select_, nullptr);
		if (e != SQLITE_OK) {
			std::cerr << "failed to prepare statement: " << query_select << ": " << e << std::endl;
			return;
		}
		e = db::PrepareStatement(db, kQueryNode, &stmt_node_);
		if (e != SQLITE_OK) {
			std::cerr << "failed to prepare statement: " << kQueryNode << ": " << e << std::endl;
			return;
		}
		e = sqlite3_prepare_v2(db, query_update, N_update, &stmt_update_, nullptr);
		if (e != SQLITE_OK)
			std::cerr << "failed to prepare statement: " << query_update << ": " << e << std::endl;
	}

	~GraphMathRewriter();

	bool Rewrite();

	bool FindNode(sqlite3_int64 pq_rowid, const char *node_name, int *node_id);

private:
	bool Process(sqlite3_int64 rowid, sqlite3_int64 pq_rowid, const char *math);

	bool Update(sqlite3_int64 rowid, const char *math);

	sqlite3_stmt *stmt_select_;
	sqlite3_stmt *stmt_node_;
	sqlite3_stmt *stmt_update_;
};

}
}

#endif
