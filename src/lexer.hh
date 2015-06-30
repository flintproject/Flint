/* -*- Mode: C++; tab-width: 4; indent-tabs-mode: t; c-basic-offset: 4 -*- vim:set ts=4 sw=4 sts=4 noet: */
#ifndef FLINT_LEXER_HH_
#define FLINT_LEXER_HH_

#include <string>

#include <boost/spirit/home/qi/detail/assign_to.hpp>

namespace flint {
namespace lexer {

struct Real {
	std::string lexeme;
};

}
}

namespace boost {
namespace spirit {
namespace traits {

template<typename TIterator>
struct assign_to_attribute_from_iterators<flint::lexer::Real, TIterator>
{
	static void
	call(TIterator const& first, TIterator const& last, flint::lexer::Real& attr)
	{
		attr.lexeme = std::string(first, last);
	}
};

}
}
}

#endif
