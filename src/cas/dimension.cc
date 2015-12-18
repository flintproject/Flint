/* -*- Mode: C++; tab-width: 4; indent-tabs-mode: t; c-basic-offset: 4 -*- vim:set ts=4 sw=4 sts=4 noet: */
#include "cas/dimension.h"

#include <cassert>
#include <cstdio>
#include <iostream>
#include <memory>
#include <string>

#include "mathml.h"
#include "variable.h"
#include "variable-map.h"

using std::cerr;
using std::endl;

namespace flint {
namespace cas {

DimensionAnalyzer::DimensionAnalyzer()
	: vm_(new VariableMap)
{
}

DimensionAnalyzer::~DimensionAnalyzer() = default;

bool DimensionAnalyzer::Load(sqlite3 *db)
{
	return vm_->Load(db);
}

namespace {

bool IsUnary(const Compound &c)
{
	size_t s = c.children.size();
	if (s != 1) {
		cerr << '<' << c.keyword << "> got " << s << " arguments" << endl;
		return false;
	}
	return true;
}

bool IsBinary(const Compound &c)
{
	size_t s = c.children.size();
	if (s != 2) {
		cerr << '<' << c.keyword << "> got " << s << " arguments" << endl;
		return false;
	}
	return true;
}

class Context {
public:
	Context(VariableMap *vm, const boost::uuids::uuid &uuid)
		: vm_(vm)
		, uuid_(uuid)
	{
	}

	bool Analyse(Expr *expr, int *col, int *row);

	bool Diff(Compound *c)
	{
		assert(c->children.size() == 2);
		int col, row;
		if (!Analyse(&c->children[1], &col, &row))
			return false;
		c->col = col;
		c->row = row;
		return true;
	}

	bool BinaryEquality(Compound *c)
	{
		if (!IsBinary(*c))
			return false;
		int col0, row0;
		if (!Analyse(&c->children[0], &col0, &row0))
			return false;
		int col1, row1;
		if (!Analyse(&c->children[1], &col1, &row1))
			return false;
		if (col0 != col1) {
			cerr << "col mismatch between <" << c->keyword << ">'s RHS and LHS" << endl;
			return false;
		}
		if (row0 != row1) {
			cerr << "row mismatch between <" << c->keyword << ">'s RHS and LHS" << endl;
			return false;
		}
		c->col = c->row = 1;
		return true;
	}

	bool Plus(Compound *c)
	{
		if (c->children.size() == 0) {
			cerr << "empty arguments of <plus>" << endl;
			return false;
		}
		int col, row;
		if (!HaveSameDimension(&c->children, &col, &row))
			return false;
		c->col = col;
		c->row = row;
		return true;
	}

	bool Minus(Compound *c)
	{
		if (c->children.size() == 0) {
			cerr << "empty arguments of <minus>" << endl;
			return false;
		}
		int col, row;
		if (!HaveSameDimension(&c->children, &col, &row))
			return false;
		c->col = col;
		c->row = row;
		return true;
	}

	bool Times(Compound *c)
	{
		if (c->children.size() == 0) {
			cerr << "empty arguments of <times>" << endl;
			return false;
		}
		int col, row;
		if (!Multiplication(c->children.begin(), c->children.end(),
							  &col, &row))
			return false;
		c->col = col;
		c->row = row;
		return true;
	}

	bool Log(Compound *c)
	{
		size_t s = c->children.size();
		switch (s) {
		case 1: // base 10
			if (!Scalar(&c->children[0]))
				return false;
			c->col = c->row = 1;
			return true;
		case 2:
			if (!Scalar(&c->children[0]))
				return false;
			if (!Scalar(&c->children[1]))
				return false;
			c->col = c->row = 1;
			return true;
		default:
			cerr << "<log> got " << s << "arguments" << endl;
			return false;
		}
	}

	bool Power(Compound *c)
	{
		if (!IsBinary(*c))
			return false;
		if (!Scalar(&c->children[1]))
			return false;
		int n;
		if (!Square(&c->children[0], &n))
			return false;
		c->col = c->row = n;
		return true;
	}

	bool Root(Compound *c)
	{
		size_t s = c->children.size();
		switch (s) {
		case 1: // default degree 2
			if (!Scalar(&c->children[0]))
				return false;
			c->col = c->row = 1;
			return true;
		case 2:
			if (!Scalar(&c->children[0]))
				return false;
			if (!Scalar(&c->children[1]))
				return false;
			c->col = c->row = 1;
			return true;
		default:
			cerr << "<root> got " << s << "arguments" << endl;
			return false;
		}
	}

	bool Matrix(Compound *c)
	{
		if (c->children.empty()) {
			cerr << "empty <matrix> found" << endl;
			return false;
		}
		int col0;
		if (!Matrixrow(&c->children.at(0), &col0))
			return false;
		for (auto it=c->children.begin()+1;it!=c->children.end();++it) { // from 2nd
			int col;
			if (!Matrixrow(&(*it), &col))
				return false;
			if (col != col0) {
				cerr << "column numbers of <matrixrow>s are not aligned in <matrix>" << endl; // TODO
				return false;
			}
		}
		c->col = col0;
		c->row = static_cast<int>(c->children.size());
		return true;
	}

	bool Determinant(Compound *c)
	{
		if (!IsUnary(*c))
			return false;
		int col0, row0;
		if (!Analyse(&c->children[0], &col0, &row0))
			return false;
		if (col0 != row0) {
			cerr << "col/row mismatch for <determinant>: "
				 << col0 << '/' << row0
				 << endl;
			return false;
		}
		c->col = c->row = 1;
		return true;
	}

	bool Vector(Compound *c)
	{
		if (c->children.empty()) {
			cerr << "empty <vector> found" << endl;
			return false;
		}
		for (auto &child : c->children) {
			if (!Scalar(&child))
				return false;
		}
		c->col = 1;
		c->row = static_cast<int>(c->children.size());
		return true;
	}

	bool Selector(Compound *c)
	{
		int col0, row0;
		switch (c->children.size()) {
		case 0:
			cerr << "empty <selector> found" << endl;
			return false;
		case 1:
			// TODO
			cerr << "unary <selector> is not supported yet" << endl;
			return false;
		case 2:
			if (!Analyse(&c->children[0], &col0, &row0))
				return false;
			if (!Scalar(&c->children[1]))
				return false;
			if (col0 > 1 && row0 > 1) { // matrix
				c->col = col0;
				c->row = 1;
			} else {
				c->col = c->row = 1;
			}
			return true;
		case 3:
			if (!Analyse(&c->children[0], &col0, &row0))
				return false;
			if (!Scalar(&c->children[1]))
				return false;
			if (!Scalar(&c->children[2]))
				return false;
			c->col = c->row = 1;
			return true;
		default:
			cerr << "more than 3 arguments for <selector>" << endl;
			return false;
		}
	}

	bool Transpose(Compound *c)
	{
		if (!IsUnary(*c))
			return false;
		int col0, row0;
		if (!Analyse(&c->children[0], &col0, &row0))
			return false;
		c->col = row0;
		c->row = col0;
		return true;
	}

	bool Vectorproduct(Compound *c)
	{
		if (!IsBinary(*c))
			return false;
		int col0, row0;
		if (!Analyse(&c->children[0], &col0, &row0))
			return false;
		int col1, row1;
		if (!Analyse(&c->children[1], &col1, &row1))
			return false;
		if (col0 != col1) {
			cerr << "col mismatch between two arguments of <vectorproduct>: "
				 << col0 << " vs " << col1 << endl;
			return false;
		}
		if (row0 != row1) {
			cerr << "row mismatch between two arguments of <vectorproduct>: "
				 << row0 << " vs " << row1 << endl;
			return false;
		}
		if (col0 == 3 && row0 == 1) {
			c->col = 3;
			c->row = 1;
			return true;
		}
		if (col0 == 1 && row0 == 3) {
			c->col = 1;
			c->row = 3;
			return true;
		}
		cerr << "<vectorproduct> got a non-3-dimensional vector: "
			 << col0 << '/' << row0 << endl;
		return false;
	}

	bool UnaryScalar(Compound *c)
	{
		if (!IsUnary(*c))
			return false;
		if (!Scalar(&c->children[0]))
			return false;
		c->col = c->row = 1;
		return true;
	}

	bool BinaryScalar(Compound *c)
	{
		if (!IsBinary(*c))
			return false;
		if (!Scalar(&c->children[0]))
			return false;
		if (!Scalar(&c->children[1]))
			return false;
		c->col = c->row = 1;
		return true;
	}

	bool NaryScalar(Compound *c)
	{
		for (auto &child : c->children) {
			if (!Scalar(&child))
				return false;
		}
		c->col = c->row = 1;
		return true;
	}

	bool Piecewise(Compound *c)
	{
		if (c->children.size() == 0) {
			cerr << "empty <piecewise>" << endl;
			return false;
		}
		int col, row;
		if (!HaveSameDimension(&c->children, &col, &row))
			return false;
		c->col = col;
		c->row = row;
		return true;
	}

	bool Piece(Compound *c)
	{
		if (!IsBinary(*c))
			return false;
		int col0, row0;
		if (!Analyse(&c->children[0], &col0, &row0))
			return false;
		int col1, row1;
		if (!Analyse(&c->children[1], &col1, &row1))
			return false;
		if (col1 != 1) {
			cerr << "condition part of <piece> must be scalar, but given " << col1
				 << " as the number of columns" << endl;
			return false;
		}
		if (row1 != 1) {
			cerr << "condition part of <piece> must be scalar, but given " << row1
				 << " as the number of rows" << endl;
			return false;
		}
		c->col = col0;
		c->row = row0;
		return true;
	}

	bool Otherwise(Compound *c)
	{
		if (!IsUnary(*c))
			return false;
		int col, row;
		if (!Analyse(&c->children[0], &col, &row))
			return false;
		c->col = col;
		c->row = row;
		return true;
	}

private:
	bool HaveSameDimension(std::vector<Expr> *children, int *col, int *row)
	{
		assert(!children->empty());
		int c0, r0;
		if (!Analyse(&(*children)[0], &c0, &r0))
			return false;
		for (auto it=children->begin()+1;it!=children->end();++it) { // from 2nd
			int c, r;
			if (!Analyse(&(*it), &c, &r))
				return false;
			if (c != c0) {
				cerr << "inconsistent col" << endl; // TODO
				return false;
			}
			if (r != r0) {
				cerr << "inconsistent row" << endl; // TODO
				return false;
			}
		}
		*col = c0;
		*row = r0;
		return true;
	}

	bool Scalar(Expr *expr)
	{
		int col, row;
		if (!Analyse(expr, &col, &row))
			return false;
		if (col != 1) {
			cerr << "scalar expected, but (col, row) = ("
				 << col << ", " << row
				 << ')' << endl;
			return false;
		}
		if (row != 1) {
			cerr << "scalar expected, but (col, row) = ("
				 << col << ", " << row
				 << ')' << endl;
			return false;
		}
		return true;
	}

	bool Square(Expr *expr, int *n)
	{
		int col, row;
		if (!Analyse(expr, &col, &row))
			return false;
		if (col != row) {
			cerr << "col expected to equal row, but (col, row) = ("
				 << col << ", " << row
				 << ')' << endl;
			return false;
		}
		*n = col;
		return true;
	}

	bool Multiplication(std::vector<Expr>::iterator bit,
						std::vector<Expr>::iterator eit,
						int *col, int *row)
	{
		int c0, r0;
		if (!Analyse(&(*bit), &c0, &r0))
			return false;
		if (++bit == eit) {
			*col = c0;
			*row = r0;
			return true;
		}
		int c1, r1;
		if (!Multiplication(bit, eit, &c1, &r1))
			return false;
		if (c0 != r1) {
			cerr << "col/row mismatch for multiplication" << endl; // TODO
			return false;
		}
		*col = c1;
		*row = r0;
		return true;
	}

	bool Matrixrow(Expr *expr, int *col)
	{
		if (expr->which() != static_cast<int>(ExprType::kCompound)) {
			cerr << "expected <matrixrow>, but got something else" << endl; // TODO
			return false;
		}
		Compound &c = boost::get<Compound>(*expr);
		if (c.keyword != "matrixrow") {
			cerr << "expected <matrixrow>, but got something else" << endl; // TODO
			return false;
		}
		size_t size = c.children.size();
		if (size == 0) {
			cerr << "empty <matrixrow> found" << endl;
			return false;
		}
		for (auto &child : c.children) {
			if (!Scalar(&child))
				return false;
		}
		*col = c.col = static_cast<int>(size);
		c.row = 1;
		return true;
	}

	VariableMap *vm_;
	boost::uuids::uuid uuid_;
};

struct KeyFun {
	std::string keyword;
	bool (Context::*function)(Compound *);
};

const KeyFun kKeyFun[] = {
	// Keep the following entries in bibliographical order.
	{"abs", &Context::UnaryScalar},
	{"and", &Context::NaryScalar},
	{"arccos", &Context::UnaryScalar},
	{"arccosh", &Context::UnaryScalar},
	{"arccot", &Context::UnaryScalar},
	{"arccoth", &Context::UnaryScalar},
	{"arccsc", &Context::UnaryScalar},
	{"arccsch", &Context::UnaryScalar},
	{"arcsec", &Context::UnaryScalar},
	{"arcsech", &Context::UnaryScalar},
	{"arcsin", &Context::UnaryScalar},
	{"arcsinh", &Context::UnaryScalar},
	{"arctan", &Context::UnaryScalar},
	{"arctanh", &Context::UnaryScalar},
	{"ceiling", &Context::UnaryScalar},
	{"cos", &Context::UnaryScalar},
	{"cosh", &Context::UnaryScalar},
	{"cot", &Context::UnaryScalar},
	{"coth", &Context::UnaryScalar},
	{"csc", &Context::UnaryScalar},
	{"csch", &Context::UnaryScalar},
	{"diff", &Context::Diff},
	{"divide", &Context::BinaryScalar},
	{"determinant", &Context::Determinant},
	{"eq", &Context::BinaryEquality},
	{"exp", &Context::UnaryScalar},
	{"floor", &Context::UnaryScalar},
	{"geq", &Context::BinaryScalar},
	{"gt", &Context::BinaryScalar},
	{"leq", &Context::BinaryScalar},
	{"ln", &Context::UnaryScalar},
	{"log", &Context::Log},
	{"lt", &Context::BinaryScalar},
	{"matrix", &Context::Matrix},
	{"max", &Context::BinaryScalar},
	{"mean", &Context::NaryScalar},
	{"min", &Context::BinaryScalar},
	{"minus", &Context::Minus},
	{"neq", &Context::BinaryEquality},
	{"not", &Context::UnaryScalar},
	{"or", &Context::NaryScalar},
	{"otherwise", &Context::Otherwise},
	{"piece", &Context::Piece},
	{"piecewise", &Context::Piecewise},
	{"plus", &Context::Plus},
	{"power", &Context::Power},
	{"rem", &Context::BinaryScalar},
	{"root", &Context::Root},
	{"sec", &Context::UnaryScalar},
	{"sech", &Context::UnaryScalar},
	{"selector", &Context::Selector},
	{"sin", &Context::UnaryScalar},
	{"sinh", &Context::UnaryScalar},
	{"tan", &Context::UnaryScalar},
	{"tanh", &Context::UnaryScalar},
	{"times", &Context::Times},
	{"transpose", &Context::Transpose},
	{"vector", &Context::Vector},
	{"vectorproduct", &Context::Vectorproduct},
	{"xor", &Context::NaryScalar}
};

bool Context::Analyse(Expr *expr, int *col, int *row)
{
	assert(expr);
	int type = expr->which();
	if (type == static_cast<int>(ExprType::kCompound)) {
		Compound &c = boost::get<Compound>(*expr);
		for (const auto &kf : kKeyFun) { // TODO: faster search e.g. via bsearch?
			if (c.keyword == kf.keyword) {
				if (!(this->*(kf.function))(&c))
					return false;
				*col = c.col;
				*row = c.row;
				return true;
			}
		}
		cerr << "unsupported function: " << c.keyword << endl;
		return false;
	} else if (type == static_cast<int>(ExprType::kString)) {
		std::string name = boost::get<std::string>(*expr);
		assert(!name.empty());
		if (name[0] == '%') {
			std::string name1 = name.substr(1);
			if (name1 == "time") { // the global variable "time"
				*col = *row = 1;
				return true;
			}
			const Variable *v = vm_->Find(uuid_, name1);
			if (!v) {
				cerr << "failed to find variable: " << name1 << endl;
				return false;
			}
			*col = v->col();
			*row = v->row();
		} else if (name == "@dt") {
			*col = *row = 1;
		} else if (mathml::IsScalarConstantElement(name)) {
			*col = *row = 1;
		} else { // TODO
			assert(false);
		}
	} else if (type == static_cast<int>(ExprType::kInteger)) {
		*col = *row = 1;
	} else {
		assert(type == static_cast<int>(ExprType::kReal));
		*col = *row = 1;
	}
	return true;
}

}

bool DimensionAnalyzer::Analyse(const boost::uuids::uuid &uuid, Expr *expr,
								int *col, int *row)
{
	assert(expr);
	std::unique_ptr<Context> context(new Context(vm_.get(), uuid));
	return context->Analyse(expr, col, row);
}

}
}
