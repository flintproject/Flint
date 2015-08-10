/* -*- Mode: C++; tab-width: 4; indent-tabs-mode: t; c-basic-offset: 4 -*- vim:set ts=4 sw=4 sts=4 noet: */
#include "method/helper.hh"

namespace flint {
namespace method {

void RewriteDeltaTime(Compound &x, const std::string &id)
{
	if (id == "%time") {
		x.keyword = "minus";
		x.children.push_back("%time");
		x.children.push_back("@dt");
	} else {
		x.keyword = "$lookback";
		x.children.push_back(id);

		Compound c;
		c.keyword = "minus";
		c.children.push_back("%time");
		c.children.push_back("@dt");
		x.children.push_back(c);
	}
}

}
}
