/* -*- Mode: C++; tab-width: 4; indent-tabs-mode: t; c-basic-offset: 4 -*- vim:set ts=4 sw=4 sts=4 noet: */
#ifndef FLINT_CAS_AST_H_
#define FLINT_CAS_AST_H_

#include <cstdio>
#include <cstdlib>
#include <cstring>

#include "sexp.h"

// Abstract syntax tree
class Ast {
public:
	Ast(const char *id, const Sexp *tree) : id_(strdup(id)), tree_(tree) {}

	~Ast() {free(id_);}

	const char *id() const {return id_;}

	void Print(const char *uuid) const {
		std::printf("%s %s ", uuid, id_);
		tree_->Print();
		std::putchar('\n');
	}

private:
	char *id_;
	const Sexp *tree_;
};

#endif
