/* -*- Mode: C++; tab-width: 4; indent-tabs-mode: t; c-basic-offset: 4 -*- vim:set ts=4 sw=4 sts=4 noet: */
#ifdef HAVE_CONFIG_H
#include "config.h"
#endif

#include "compiler/bcc/token.h"

#include <cassert>

namespace flint {
namespace compiler {
namespace bcc {

std::ostream &Token::Write(std::ostream &os) const
{
	assert(type != Type::kUnspecified);
	return os.write(lexeme, size);
}

}
}
}
