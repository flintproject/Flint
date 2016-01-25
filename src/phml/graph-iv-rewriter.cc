/* -*- Mode: C++; tab-width: 4; indent-tabs-mode: t; c-basic-offset: 4 -*- vim:set ts=4 sw=4 sts=4 noet: */
#include "graph-iv-rewriter.h"

#include <cassert>
#include <cstdlib>
#include <cstring>
#include <memory>
#include <boost/spirit/include/phoenix.hpp>
#include <boost/spirit/include/qi.hpp>
#include <boost/uuid/uuid_io.hpp>

using std::cerr;
using std::endl;
using std::memcpy;
using std::string;

namespace flint {

namespace phml {

GraphIvRewriter::GraphIvRewriter()
	: stmt_graph_(nullptr)
	, stmt_node_(nullptr)
	, stmt_update_(nullptr)
{
}

GraphIvRewriter::~GraphIvRewriter()
{
	sqlite3_finalize(stmt_graph_);
	sqlite3_finalize(stmt_node_);
	sqlite3_finalize(stmt_update_);
}

namespace {

const char kQueryGraph[] = \
	"SELECT p.rowid, m.module_id, p.name, i.math FROM ivs AS i"
	" LEFT JOIN pqs AS p ON i.pq_rowid = p.rowid"
	" LEFT JOIN modules AS m ON p.module_rowid = m.rowid"
	" WHERE EXISTS (SELECT * FROM nodes AS n WHERE p.rowid = n.pq_rowid)";

const char kQueryNode[] = "SELECT node_id FROM nodes WHERE pq_rowid = ? AND name = ?";

const char kQueryUpdate[] = "UPDATE ivs SET math = ? WHERE pq_rowid = ?";

} // namespace

bool GraphIvRewriter::Rewrite(sqlite3 *db)
{
	int e = sqlite3_prepare_v2(db, kQueryGraph, -1, &stmt_graph_, NULL);
	if (e != SQLITE_OK) {
		cerr << "failed to prepare statement: " << kQueryGraph
			 << ": " << e << endl;
		return false;
	}
	e = sqlite3_prepare_v2(db, kQueryNode, -1, &stmt_node_, NULL);
	if (e != SQLITE_OK) {
		cerr << "failed to prepare statement: " << kQueryNode
			 << ": " << e << endl;
		return false;
	}
	e = sqlite3_prepare_v2(db, kQueryUpdate, -1, &stmt_update_, NULL);
	if (e != SQLITE_OK) {
		cerr << "failed to prepare statement: " << kQueryUpdate
			 << ": " << e << endl;
		return false;
	}

	for (e = sqlite3_step(stmt_graph_); e == SQLITE_ROW; e = sqlite3_step(stmt_graph_)) {
		sqlite3_int64 pq_rowid = sqlite3_column_int64(stmt_graph_, 0);
		const void *module_id = sqlite3_column_blob(stmt_graph_, 1);
		const unsigned char *name = sqlite3_column_text(stmt_graph_, 2);
		const unsigned char *math = sqlite3_column_text(stmt_graph_, 3);
		assert(module_id);
		boost::uuids::uuid u;
		memcpy(&u, module_id, u.size());
		if (!Process(pq_rowid,
					 u,
					 (const char *)name,
					 (const char *)math)) return false;
	}
	if (e != SQLITE_DONE) {
		cerr << "failed to step statement: " << kQueryGraph << ": " << e << endl;
		return false;
	}
	for (Map::const_iterator it=m_.begin();it!=m_.end();++it) {
		if (!Update(it->first, it->second.c_str())) return false;
	}
	return true;
}

bool GraphIvRewriter::Process(sqlite3_int64 pq_rowid,
							  const boost::uuids::uuid &module_id,
							  const char *name,
							  const char *math)
{
	using boost::phoenix::push_back;
	using boost::spirit::_1;
	using boost::spirit::qi::char_;
	using boost::spirit::qi::graph;
	using boost::spirit::qi::parse;

	const char *p = math;
	size_t len = std::strlen(math);
	string lhs;
	string rhs;
	bool r = parse(p, math + len,
				   *char_(' ')
				   >> "($is %"
				   >> +((graph - ')')[push_back(boost::phoenix::ref(lhs), _1)])
				   >> " %"
				   >> +((graph - ')')[push_back(boost::phoenix::ref(rhs), _1)])
				   >> ')');
	if (!r || p != math + len) {
		cerr << "failed to parse the definition of "
			 << module_id << ':' << name
			 << "'s initial-value: " << math << endl;
		return false;
	}
	if (std::strcmp(name, lhs.c_str()) != 0) {
		cerr << "unexpected LHS of the definition of "
			 << module_id << ':' << name
			 << "'s initial-value: " << math << endl;
		return false;
	}
	int node_id;
	if (!FindNode(pq_rowid, rhs.c_str(), &node_id)) return false;
	std::unique_ptr<char[]> buf(new char[len + 64]); // long enough
	sprintf(buf.get(), " (eq %%%s %d)", name, node_id);
	m_.insert(std::make_pair(pq_rowid, buf.get()));
	return true;
}

bool GraphIvRewriter::FindNode(sqlite3_int64 pq_rowid,
							   const char *node_name,
							   int *node_id)
{
	int e;
	e = sqlite3_bind_int64(stmt_node_, 1, pq_rowid);
	if (e != SQLITE_OK) {
		cerr << "failed to bind pq_rowid: " << kQueryNode
			 << ": " << e << endl;
		return false;
	}
	e = sqlite3_bind_text(stmt_node_, 2, node_name, -1, SQLITE_STATIC);
	if (e != SQLITE_OK) {
		cerr << "failed to bind name: " << kQueryNode
			 << ": " << e << endl;
		return false;
	}
	e = sqlite3_step(stmt_node_);
	if (e != SQLITE_ROW) {
		cerr << "failed to find node named " << node_name
			 << ": " << kQueryNode << endl;
		return false;
	}
	int r = sqlite3_column_int(stmt_node_, 0);
	assert(r > 0);
	*node_id = r;
	sqlite3_reset(stmt_node_);
	return true;
}

bool GraphIvRewriter::Update(sqlite3_int64 pq_rowid, const char *math)
{
	int e = sqlite3_bind_text(stmt_update_, 1, math, -1, SQLITE_STATIC);
	if (e != SQLITE_OK) {
		cerr << "failed to bind math: " << kQueryUpdate
			 << ": " << e << endl;
		return false;
	}
	e = sqlite3_bind_int64(stmt_update_, 2, pq_rowid);
	if (e != SQLITE_OK) {
		cerr << "failed to bind pq_rowid: " << kQueryUpdate
			 << ": " << e << endl;
		return false;
	}
	e = sqlite3_step(stmt_update_);
	if (e != SQLITE_DONE) {
		cerr << "failed to step statement: " << kQueryUpdate
			 << ": " << e << endl;
		return false;
	}
	sqlite3_reset(stmt_update_);
	return true;
}

}
}
