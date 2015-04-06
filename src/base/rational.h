/* -*- Mode: C++; tab-width: 4; indent-tabs-mode: t; c-basic-offset: 4 -*- vim:set ts=4 sw=4 sts=4 noet: */
#ifndef BASE_RATIONAL_H_
#define BASE_RATIONAL_H_

#include <cstdio>
#include <iostream>
#include <sstream>
#include <string>
#include <boost/rational.hpp>

namespace base {

template <class T>
class Rational {
public:
	static bool FromStream(std::istream &in, boost::rational<T> &r, int *s = NULL)
	{
		T d = 0, n = 1;
		bool d_found = false;
		std::streampos t = in.tellg();

		T sign = 0;
		char c = in.peek();
		if (c == EOF) return false;
		if (c == '+') {
			in.ignore();
			sign = 1;
			c = in.peek();
			if (c == '+' || c == '-') return false;
		} else if (c == '-') {
			in.ignore();
			sign = -1;
			c = in.peek();
			if (c == '+' || c == '-') return false;
		}

		in >> d;
		if (in.fail()) { // no integer part
			in.clear();
		} else {
			d_found = true;
		}
		c = in.peek();
		if (c == '.') {
			in.ignore();
			while (in.good()) {
				char c = in.peek();
				if ('0' <= c && c <= '9') {
					in.ignore();
					d_found = true;
					d *= 10;
					d += c - '0';
					n *= 10;
				} else {
					break;
				}
			}
		}
		if (!d_found) return false;
		c = in.peek();
		if (c == 'e' || c == 'E') {
			in.ignore();
			T e = 0;
			in >> e;
			if (in.fail()) { // no exponent
				in.clear();
				return false;
			}
			if (e == 0) {
				/* nothing to do */
			} else if (e > 0) {
				do {
					d *= 10;
				} while (--e > 0);
			} else {
				do {
					n *= 10;
				} while (++e < 0);
			}
		}
		r = boost::rational<T>((sign) ? sign * d : d, n);
		if (s) *s = static_cast<int>(in.tellg() - t);
		return true;
	}

	static bool FromString(const std::string &s, boost::rational<T> &r)
	{
		std::istringstream iss(s);
		return FromStream(iss, r);
	}
};

} // namespace base

#endif // BASE_RATIONAL_H_
