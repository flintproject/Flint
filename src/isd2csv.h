/* -*- Mode: C++; tab-width: 4; indent-tabs-mode: t; c-basic-offset: 4 -*- vim:set ts=4 sw=4 sts=4 noet: */
#ifndef FLINT_ISD2CSV_H_
#define FLINT_ISD2CSV_H_

#include <iostream>
#ifdef ENABLE_TCP
#include <string>
#endif

namespace flint {
namespace isd2csv {

struct Option {
	bool ignore_prefixes;
	bool ignore_units;
#ifdef ENABLE_TCP
	std::string port;
#endif
};

int Convert(const Option &option, std::istream *input, std::ostream *output);

}
}

#endif
