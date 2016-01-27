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

class Context {
public:
	Context(const boost::uuids::uuid &uuid,
			const char *id,
			std::ostream *os);

	~Context();

	int get_avail_n() const {return avail_n_;}

	bool EmitCode(cas::Expr &sexp);

private:
	bool EmitCondition(int n, int l, cas::Expr &sexp);

	bool EmitAt(int n, std::deque<cas::Expr> &children);

	bool EmitLookback(int n, std::deque<cas::Expr> &children);

	bool EmitPiecewise(int n, std::deque<cas::Expr> &children);

	bool EmitTrial(int n, std::deque<cas::Expr> &children);

	bool EmitCode(int n, cas::Expr &sexp);

	boost::uuids::uuid uuid_;
	const char *id_;
	int avail_n_;
	int avail_l_;
	std::ostream *os_;
};

}
}
}

#endif
