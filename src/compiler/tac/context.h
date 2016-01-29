/* -*- Mode: C++; tab-width: 4; indent-tabs-mode: t; c-basic-offset: 4 -*- vim:set ts=4 sw=4 sts=4 noet: */
#ifndef FLINT_COMPILER_TAC_CONTEXT_H_
#define FLINT_COMPILER_TAC_CONTEXT_H_

#include <deque>
#include <iostream>

#include <boost/uuid/uuid.hpp>

#include "cas.h"

namespace flint {
namespace compiler {
namespace tac {

enum class RegisterType {
	kInteger,
	kFloat
};

class Context {
public:
	Context(const boost::uuids::uuid &uuid,
			const char *id,
			std::ostream *os);

	~Context();

	int get_ir() const {return ir_;}
	int get_fr() const {return fr_;}

	bool EmitCode(cas::Expr &sexp);

	bool Componentwise(RegisterType rt, int n, cas::Compound &c);
	bool NaryScalarToScalar(RegisterType rt, int n, cas::Compound &c);
	bool Determinant(RegisterType rt, int n, cas::Compound &c);
	bool Matrix(RegisterType rt, int n, cas::Compound &c);
	bool Outerproduct(RegisterType rt, int n, cas::Compound &c);
	bool Scalarproduct(RegisterType rt, int n, cas::Compound &c);
	bool Selector(RegisterType rt, int n, cas::Compound &c);
	bool Times(RegisterType rt, int n, cas::Compound &c);
	bool Transpose(RegisterType rt, int n, cas::Compound &c);
	bool Vector(RegisterType rt, int n, cas::Compound &c);
	bool Vectorproduct(RegisterType rt, int n, cas::Compound &c);

private:
	bool EmitCondition(int n, int l, cas::Expr &sexp);
	bool EmitAt(int n, std::deque<cas::Expr> &children);
	bool EmitLookback(int n, std::deque<cas::Expr> &children);
	bool Piecewise(RegisterType rt, int n, std::deque<cas::Expr> &children);
	bool EmitTrial(int n, std::deque<cas::Expr> &children);
	bool Assign(RegisterType rt, int n, cas::Expr &expr);
	bool Assign(RegisterType rt, int n, cas::Compound &c);
	bool Assign(RegisterType rt, int n, const cas::Identifier &id);
	bool Assign(int n, int i);
	bool Assign(int n, const lexer::Real &r);
	void GetType(cas::Expr &expr, int *w, int *col, int *row);

	boost::uuids::uuid uuid_;
	const char *id_;
	int ir_; // integer registers
	int fr_; // floating-point registers
	int l_; // labels
	std::ostream *os_;
};

}
}
}

#endif
