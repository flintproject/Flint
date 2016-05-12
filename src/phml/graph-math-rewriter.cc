/* -*- Mode: C++; tab-width: 4; indent-tabs-mode: t; c-basic-offset: 4 -*- vim:set ts=4 sw=4 sts=4 noet: */
#include "graph-math-rewriter.h"

#include <cassert>
#include <cstdlib>
#include <cstring>
#include <iostream>
#include <memory>
#include <sstream>
#include <string>
#include <vector>

#include "flint/parser.h"
#include "flint/sexp.h"

using std::cerr;
using std::endl;

namespace flint {
namespace {

struct Detector : public sexp::Visitor<bool>
{
	bool operator()(const sexp::Identifier &/*x*/) override {return false;}

	bool operator()(const sexp::Literal &/*a*/) override {return false;}

	bool operator()(const sexp::Compound &c) override {
		const auto &children = c.children();
		size_t s = children.size();
		assert(s > 0);
		const auto &head = children.at(0);
		if (head->type() == sexp::Expression::Type::kIdentifier) {
			const Token &t = static_cast<const sexp::Identifier *>(head.get())->token();
			if (t.Equals("$is"))
				return true;
		}
		for (size_t i=1;i<s;i++) {
			if (sexp::ApplyVisitor(*this, *children.at(i)))
				return true;
		}
		return false;
	}
};

class Writer : public sexp::Visitor<bool>
{
public:
	Writer(phml::GraphMathRewriter *rewriter,
					 sqlite3_int64 pq_rowid,
					 std::ostringstream *oss)
		: rewriter_(rewriter),
		  pq_rowid_(pq_rowid),
		  oss_(oss)
	{
	}

	bool operator()(const sexp::Identifier &x) override {
		return bool(x.Write(oss_));
	}

	bool operator()(const sexp::Literal &a) override {
		return bool(a.Write(oss_));
	}

	bool operator()(const sexp::Compound &c) override {
		const auto &children = c.children();
		size_t s = children.size();
		assert(s > 0);
		const auto &head = children.at(0);
		if (head->type() != sexp::Expression::Type::kIdentifier)
			return RewriteRecursively(c);
		const Token &t = static_cast<const sexp::Identifier *>(head.get())->token();
		if (!t.Equals("$is"))
			return RewriteRecursively(c);
		assert(s == 3);
		const auto &lhs = children.at(1);
		const auto &rhs = children.at(2);
		if (rhs->type() != sexp::Expression::Type::kIdentifier) {
			std::cerr << "invalid 2nd argument of $is: ";
			rhs->Write(&std::cerr);
			std::cerr << std::endl;
			return false;
		}
		const Token &t2 = static_cast<const sexp::Identifier *>(rhs.get())->token();
		// copy lexeme, but skip the first %
		std::unique_ptr<char[]> node_name(new char[t2.size]);
		std::memcpy(node_name.get(), t2.lexeme+1, t2.size-1);
		node_name[t2.size-1] = '\0';
		int node_id;
		if (!rewriter_->FindNode(pq_rowid_, node_name.get(), &node_id))
			return false;
		*oss_ << "(eq ";
		lhs->Write(oss_);
		oss_->put(' ');
		*oss_ << node_id;
		return bool(oss_->put(')'));
	}

private:
	bool RewriteRecursively(const sexp::Compound &c) {
		const auto &children = c.children();
		size_t s = children.size();
		oss_->put('(');
		for (size_t i=0;i<s;i++) {
			if (i > 0)
				oss_->put(' ');
			if (!sexp::ApplyVisitor(*this, *children.at(i)))
				return false;
		}
		return bool(oss_->put(')'));
	}

	phml::GraphMathRewriter *rewriter_;
	sqlite3_int64 pq_rowid_;
	std::ostringstream *oss_;
};

const char kQueryNode[] = \
	"SELECT n.node_id FROM nodes AS n"
	" LEFT JOIN pqs AS p ON n.pq_rowid = p.rowid"
	" LEFT JOIN modules AS m ON p.module_rowid = m.rowid"
	" WHERE n.name = ?"
	" AND EXISTS (SELECT * FROM pqs WHERE rowid = ? AND module_rowid = m.rowid)";

} // namespace

namespace phml {

GraphMathRewriter::GraphMathRewriter(const char *query_select,
									 const char *query_update)
	: query_select_(query_select)
	, query_update_(query_update)
	, stmt_select_(nullptr)
	, stmt_node_(nullptr)
	, stmt_update_(nullptr)
{
}

GraphMathRewriter::~GraphMathRewriter()
{
	sqlite3_finalize(stmt_select_);
	sqlite3_finalize(stmt_node_);
	sqlite3_finalize(stmt_update_);
}

bool GraphMathRewriter::Rewrite(sqlite3 *db)
{
	int e = sqlite3_prepare_v2(db, query_select_, -1, &stmt_select_, nullptr);
	if (e != SQLITE_OK) {
		cerr << "failed to prepare statement: " << query_select_ << ": " << e << endl;
		return false;
	}
	e = sqlite3_prepare_v2(db, kQueryNode, -1, &stmt_node_, nullptr);
	if (e != SQLITE_OK) {
		cerr << "failed to prepare statement: " << kQueryNode << ": " << e << endl;
		return false;
	}
	e = sqlite3_prepare_v2(db, query_update_, -1, &stmt_update_, nullptr);
	if (e != SQLITE_OK) {
		cerr << "failed to prepare statement: " << query_update_ << ": " << e << endl;
		return false;
	}

	for (e = sqlite3_step(stmt_select_); e == SQLITE_ROW; e = sqlite3_step(stmt_select_)) {
		sqlite3_int64 rowid = sqlite3_column_int64(stmt_select_, 0);
		sqlite3_int64 pq_rowid = sqlite3_column_int64(stmt_select_, 1);
		const unsigned char *math = sqlite3_column_text(stmt_select_, 2);
		if (!Process(rowid, pq_rowid, (const char *)math)) return false;
	}
	if (e != SQLITE_DONE) {
		cerr << "failed to step statement: " << query_select_ << ": " << e << endl;
		return false;
	}
	return true;
}

bool GraphMathRewriter::Process(sqlite3_int64 rowid,
								sqlite3_int64 pq_rowid,
								const char *math)
{
	std::unique_ptr<sexp::Expression> expr;
	parser::Parser parser(math);
	if (!parser(&expr))
		return false;
	Detector d;
	if (!sexp::ApplyVisitor(d, *expr))
		return true;
	std::ostringstream oss;
	oss.put(' '); // a leading space
	Writer w(this, pq_rowid, &oss);
	if (!sexp::ApplyVisitor(w, *expr))
		return false;
	return Update(rowid, oss.str().c_str());
}

bool GraphMathRewriter::FindNode(sqlite3_int64 pq_rowid, const char *node_name, int *node_id)
{
	int e;
	e = sqlite3_bind_text(stmt_node_, 1, node_name, -1, SQLITE_STATIC);
	if (e != SQLITE_OK) {
		cerr << "failed to bind name: " << kQueryNode
			 << ": " << e << endl;
		return false;
	}
	e = sqlite3_bind_int64(stmt_node_, 2, pq_rowid);
	if (e != SQLITE_OK) {
		cerr << "failed to bind pq_rowid: " << kQueryNode
			 << ": " << e << endl;
		return false;
	}
	e = sqlite3_step(stmt_node_);
	if (e != SQLITE_ROW) {
		cerr << "failed to find node with pq_rowid/named: "
			 << pq_rowid << '/' << node_name
			 << ": " << kQueryNode
			 << ": " << e << endl;
		return false;
	}
	int r = sqlite3_column_int(stmt_node_, 0);
	assert(r > 0);
	*node_id = r;
	sqlite3_reset(stmt_node_);
	return true;
}

bool GraphMathRewriter::Update(sqlite3_int64 rowid, const char *math)
{
	int e = sqlite3_bind_text(stmt_update_, 1, math, -1, SQLITE_STATIC);
	if (e != SQLITE_OK) {
		cerr << "failed to bind math: " << query_update_
			 << ": " << e << endl;
		return false;
	}
	e = sqlite3_bind_int64(stmt_update_, 2, rowid);
	if (e != SQLITE_OK) {
		cerr << "failed to bind rowid: " << query_update_
			 << ": " << e << endl;
		return false;
	}
	e = sqlite3_step(stmt_update_);
	if (e != SQLITE_DONE) {
		cerr << "failed to step statement: " << query_update_
			 << ": " << e << endl;
		return false;
	}
	sqlite3_reset(stmt_update_);
	return true;
}

}
}
