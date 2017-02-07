/* -*- Mode: C++; tab-width: 4; indent-tabs-mode: t; c-basic-offset: 4 -*- vim:set ts=4 sw=4 sts=4 noet: */
#ifndef FLINT_GUI_FORMULA_TOKEN_H_
#define FLINT_GUI_FORMULA_TOKEN_H_

#include <iostream>
#include <string>

namespace flint {
namespace gui {
namespace formula {

struct Token {

	enum class Type {
		kUnspecified,
		kIdentifier,
		kNumber,
		kParenOpen,
		kParenClose,
		kComma,
		kPercent,
		kStar,
		kPlus,
		kMinus,
		kSlash
	};

	Type type;
	const char *lexeme;
	int size;

	std::ostream &Write(std::ostream &os) const;
	std::string ToString() const;
};

}
}
}

#endif
