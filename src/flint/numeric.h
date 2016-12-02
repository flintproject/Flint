/* -*- Mode: C++; tab-width: 4; indent-tabs-mode: t; c-basic-offset: 4 -*- vim:set ts=4 sw=4 sts=4 noet: */
#ifndef FLINT_NUMERIC_H_
#define FLINT_NUMERIC_H_

#include <iomanip>
#include <iostream>

namespace flint {

inline void RequestMaxNumOfDigitsForDouble(std::ostream &os)
{
	// choose the defaultfloat
	os.unsetf(std::ios::floatfield);
	// See Theorem 15 of <http://docs.oracle.com/cd/E19957-01/806-3568/ncg_goldberg.html>.
	os.precision(17);
}

}

#endif
