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
		std::cerr << '<' << c.keyword << "> got " << s << " arguments" << std::endl;
		return false;
	}
	return true;
}

bool IsBinary(const Compound &c)
{
	size_t s = c.children.size();
	if (s != 2) {
		std::cerr << '<' << c.keyword << "> got " << s << " arguments" << std::endl;
		return false;
	}
	return true;
}

class Context {
public:
	Context(const VariableMap *vm, const boost::uuids::uuid &uuid)
		: vm_(vm)
		, uuid_(uuid)
	{
	}

	bool Analyse(Expr *expr, int *col, int *row);

	// TODO: support matrix/vector
	bool At(Compound *c)
	{
		assert(c->children.size() == 3);
		if (!Scalar(&c->children[0]))
			return false;
		if (!Scalar(&c->children[1]))
			return false;
		if (!Scalar(&c->children[2]))
			return false;
		c->col = 1;
		c->row = 1;
		return true;
	}

	// TODO: support matrix/vector
	bool Data(Compound *c)
	{
		assert(c->children.size() == 2);
		if (!Scalar(&c->children[0]))
			return false;
		if (!Scalar(&c->children[1]))
			return false;
		c->col = 1;
		c->row = 1;
		return true;
	}

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
			std::cerr << "col mismatch between <" << c->keyword << ">'s RHS and LHS" << std::endl;
			return false;
		}
		if (row0 != row1) {
			std::cerr << "row mismatch between <" << c->keyword << ">'s RHS and LHS" << std::endl;
			return false;
		}
		c->col = c->row = 1;
		return true;
	}

	bool Plus(Compound *c)
	{
		if (c->children.size() == 0) {
			std::cerr << "empty arguments of <plus>" << std::endl;
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
			std::cerr << "empty arguments of <minus>" << std::endl;
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
			std::cerr << "empty arguments of <times>" << std::endl;
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
			std::cerr << "<log> got " << s << "arguments" << std::endl;
			return false;
		}
	}

	bool Lookback(Compound *c)
	{
		if (!IsBinary(*c))
			return false;
		if (!Scalar(&c->children[1]))
			return false;
		int col0, row0;
		if (!Analyse(&c->children[0], &col0, &row0))
			return false;
		c->col = col0;
		c->row = row0;
		return true;
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
			std::cerr << "<root> got " << s << "arguments" << std::endl;
			return false;
		}
	}

	bool Matrix(Compound *c)
	{
		if (c->children.empty()) {
			std::cerr << "empty <matrix> found" << std::endl;
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
				std::cerr << "column numbers of <matrixrow>s are not aligned in <matrix>" << std::endl; // TODO
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
			std::cerr << "col/row mismatch for <determinant>: "
				 << col0 << '/' << row0
				 << std::endl;
			return false;
		}
		c->col = c->row = 1;
		return true;
	}

	bool Vector(Compound *c)
	{
		if (c->children.empty()) {
			std::cerr << "empty <vector> found" << std::endl;
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
			std::cerr << "empty <selector> found" << std::endl;
			return false;
		case 1:
			// TODO
			std::cerr << "unary <selector> is not supported yet" << std::endl;
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
			std::cerr << "more than 3 arguments for <selector>" << std::endl;
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

	bool Outerproduct(Compound *c)
	{
		if (!IsBinary(*c))
			return false;
		int col0, row0;
		if (!Analyse(&c->children[0], &col0, &row0))
			return false;
		int col1, row1;
		if (!Analyse(&c->children[1], &col1, &row1))
			return false;
		if (col0 != 1) {
			std::cerr << "1st argument of <outerproduct> must be a column vector, but it had "
				 << col0 << " columns" << std::endl;
			return false;
		}
		if (row1 != 1) {
			std::cerr << "2nd argument of <outerproduct> must be a row vector, but it had "
				 << row1 << " rows" << std::endl;
			return false;
		}
		c->col = col1;
		c->row = row0;
		return true;
	}

	bool Scalarproduct(Compound *c)
	{
		if (!IsBinary(*c))
			return false;
		int col, row;
		if (!HaveSameDimension(&c->children, &col, &row))
			return false;
		if (col != 1 && row != 1) {
			std::cerr << "<scalarproduct> got a non-vector: "
				 << col << '/' << row << std::endl;
			return false;
		}
		c->col = c->row = 1;
		return true;
	}

	bool Vectorproduct(Compound *c)
	{
		if (!IsBinary(*c))
			return false;
		int col, row;
		if (!HaveSameDimension(&c->children, &col, &row))
			return false;
		if (col == 3 && row == 1) {
			c->col = 3;
			c->row = 1;
			return true;
		}
		if (col == 1 && row == 3) {
			c->col = 1;
			c->row = 3;
			return true;
		}
		std::cerr << "<vectorproduct> got a non-3-dimensional vector: "
			 << col << '/' << row << std::endl;
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
			std::cerr << "empty <piecewise>" << std::endl;
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
			std::cerr << "condition part of <piece> must be scalar, but given " << col1
				 << " as the number of columns" << std::endl;
			return false;
		}
		if (row1 != 1) {
			std::cerr << "condition part of <piece> must be scalar, but given " << row1
				 << " as the number of rows" << std::endl;
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

	bool Trial(Compound *c)
	{
		if (c->children.size() == 0) {
			std::cerr << "empty $trial" << std::endl;
			return false;
		}
		int col, row;
		if (!HaveSameDimension(&c->children, &col, &row))
			return false;
		c->col = col;
		c->row = row;
		return true;
	}

private:
	bool HaveSameDimension(std::deque<Expr> *children, int *col, int *row)
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
				std::cerr << "inconsistent col: " << c0 << " vs " << c << std::endl; // TODO
				return false;
			}
			if (r != r0) {
				std::cerr << "inconsistent row: " << r0 << " vs " << r << std::endl; // TODO
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
			std::cerr << "scalar expected, but (col, row) = ("
				 << col << ", " << row
				 << ')' << std::endl;
			return false;
		}
		if (row != 1) {
			std::cerr << "scalar expected, but (col, row) = ("
				 << col << ", " << row
				 << ')' << std::endl;
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
			std::cerr << "col expected to equal row, but (col, row) = ("
				 << col << ", " << row
				 << ')' << std::endl;
			return false;
		}
		*n = col;
		return true;
	}

	bool Multiplication(std::deque<Expr>::iterator bit,
						std::deque<Expr>::iterator eit,
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
		if (c0 == 1 && r0 == 1) { // Multiplication with scalar is always OK
			*col = c1;
			*row = r1;
			return true;
		}
		if (c1 == 1 && r1 == 1) { // Another multiplication with scalar
			*col = c0;
			*row = r0;
			return true;
		}
		if (c0 != r1) {
			std::cerr << "col/row mismatch for multiplication" << std::endl; // TODO
			return false;
		}
		*col = c1;
		*row = r0;
		return true;
	}

	bool Matrixrow(Expr *expr, int *col)
	{
		if (expr->which() != kExprIsCompound) {
			std::cerr << "expected <matrixrow>, but got something else" << std::endl; // TODO
			return false;
		}
		Compound &c = boost::get<Compound>(*expr);
		if (c.keyword != "matrixrow") {
			std::cerr << "expected <matrixrow>, but got something else" << std::endl; // TODO
			return false;
		}
		size_t size = c.children.size();
		if (size == 0) {
			std::cerr << "empty <matrixrow> found" << std::endl;
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

	const VariableMap *vm_;
	boost::uuids::uuid uuid_;
};

struct KeyFun {
	std::string keyword;
	bool (Context::*function)(Compound *);
};

const KeyFun kKeyFun[] = {
	// Keep the following entries in bibliographical order.
	{"$At", &Context::At},
	{"$Data", &Context::Data},
	{"$Mod", &Context::BinaryScalar},
	{"$exponential_variate", &Context::UnaryScalar},
	{"$gamma_variate", &Context::BinaryScalar},
	{"$gauss_variate", &Context::BinaryScalar},
	{"$lognormal_variate", &Context::BinaryScalar},
	{"$lookback", &Context::Lookback},
	{"$outcome", &Context::BinaryScalar},
	{"$poisson_variate", &Context::UnaryScalar},
	{"$trial", &Context::Trial},
	{"$uniform_variate", &Context::BinaryScalar},
	{"$weibull_variate", &Context::BinaryScalar},
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
	{"determinant", &Context::Determinant},
	{"diff", &Context::Diff},
	{"divide", &Context::BinaryScalar},
	{"eq", &Context::BinaryEquality},
	{"exp", &Context::UnaryScalar},
	{"floor", &Context::UnaryScalar},
	{"geq", &Context::BinaryScalar},
	{"gt", &Context::BinaryScalar},
	{"leq", &Context::BinaryScalar},
	{"ln", &Context::UnaryScalar},
	{"log", &Context::Log},
	{"log10", &Context::UnaryScalar},
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
	{"outerproduct", &Context::Outerproduct},
	{"piece", &Context::Piece},
	{"piecewise", &Context::Piecewise},
	{"plus", &Context::Plus},
	{"power", &Context::Power},
	{"rem", &Context::BinaryScalar},
	{"root", &Context::Root},
	{"scalarproduct", &Context::Scalarproduct},
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
	if (type == kExprIsCompound) {
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
		std::cerr << "unsupported function: " << c.keyword << std::endl;
		return false;
	} else if (type == kExprIsIdentifier) {
		Identifier &id = boost::get<Identifier>(*expr);
		const std::string &name = id.name;
		assert(!name.empty());
		if (name[0] == '%') {
			std::string name1 = name.substr(1);
			if (name1 == "time") { // the global variable "time"
				*col = *row = 1;
				return true;
			}
			const Variable *v = vm_->Find(uuid_, name1);
			if (!v) {
				std::cerr << "failed to find variable: " << name1 << std::endl;
				return false;
			}
			*col = id.col = v->col();
			*row = id.row = v->row();
		} else if (name == "@dt") {
			*col = *row = id.col = id.row = 1;
		} else if (mathml::IsScalarConstantElement(name)) {
			*col = *row = id.col = id.row = 1;
		} else { // TODO
			assert(false);
		}
	} else if (type == kExprIsInteger) {
		*col = *row = 1;
	} else {
		assert(type == kExprIsReal);
		*col = *row = 1;
	}
	return true;
}

}

bool DimensionAnalyzer::Analyse(const boost::uuids::uuid &uuid, Expr *expr,
								int *col, int *row) const
{
	assert(expr);
	std::unique_ptr<Context> context(new Context(vm_.get(), uuid));
	return context->Analyse(expr, col, row);
}

}
}
