/* -*- Mode: C++; tab-width: 4; indent-tabs-mode: t; c-basic-offset: 4 -*- vim:set ts=4 sw=4 sts=4 noet: */
#ifndef FLINT_GUI_FORMULA_H_
#define FLINT_GUI_FORMULA_H_

#include <iostream>
#include <memory>

namespace flint {
namespace gui {

namespace formula {
struct Tree;
}

class Formula {
public:
	explicit Formula(formula::Tree *tree);
	~Formula();

	/*
	 * Parse UTF-8 string `input` to get a formula.
	 * In case of errors, return nullptr and output error message into `es`.
	 */
	static Formula *FromUtf8(const char *input, std::ostream &es);

	void WriteMathML(const char *prefix, std::ostream &os) const;

private:
	std::unique_ptr<formula::Tree> tree_;
};

}
}

#endif
