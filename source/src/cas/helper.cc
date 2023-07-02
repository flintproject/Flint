/* -*- Mode: C++; tab-width: 4; indent-tabs-mode: t; c-basic-offset: 4 -*- vim:set ts=4 sw=4 sts=4 noet: */
#include "cas/helper.h"

namespace flint {
namespace cas {

void RewriteDeltaTime(Compound &x, const std::string &id)
{
	if (id == "%time") {
		x.keyword = "minus";
		x.children.push_back(Identifier("%time"));
		x.children.push_back(Identifier("@dt"));
	} else {
		x.keyword = "$lookback";
		x.children.push_back(Identifier(id));

		Compound c;
		c.keyword = "minus";
		c.children.push_back(Identifier("%time"));
		c.children.push_back(Identifier("@dt"));
		x.children.push_back(c);
	}
}

}
}
