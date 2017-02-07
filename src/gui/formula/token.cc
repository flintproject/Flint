/* -*- Mode: C++; tab-width: 4; indent-tabs-mode: t; c-basic-offset: 4 -*- vim:set ts=4 sw=4 sts=4 noet: */
#ifdef HAVE_CONFIG_H
#include "config.h"
#endif

#include "gui/formula/token.h"

#include <cassert>
#include <sstream>

namespace flint {
namespace gui {
namespace formula {

std::ostream &Token::Write(std::ostream &os) const
{
	assert(type != Type::kUnspecified);
	return os.write(lexeme, size);
}

std::string Token::ToString() const
{
	std::ostringstream oss;
	Write(oss);
	return oss.str();
}

}
}
}
