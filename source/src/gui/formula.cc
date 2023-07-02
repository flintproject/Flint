/* -*- Mode: C++; tab-width: 4; indent-tabs-mode: t; c-basic-offset: 4 -*- vim:set ts=4 sw=4 sts=4 noet: */
#ifdef HAVE_CONFIG_H
#include "config.h"
#endif

#include "gui/formula.h"
#include "gui/formula/parser.h"
#include "gui/formula/tree.h"

namespace flint {
namespace gui {

Formula::Formula(formula::Tree *tree)
	: tree_(tree)
{}

Formula::~Formula() = default;

Formula *Formula::FromUtf8(const char *input, std::ostream &es)
{
	formula::Parser parser(input, es);
	std::unique_ptr<formula::Tree> tree(parser());
	if (tree)
		return new Formula(tree.release());
	return nullptr;
}

void Formula::WriteMathML(const char *prefix, std::ostream &os) const
{
	tree_->Write(prefix, os);
}

}
}
