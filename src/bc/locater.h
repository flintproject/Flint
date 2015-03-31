/* -*- Mode: C++; tab-width: 4; indent-tabs-mode: t; c-basic-offset: 4 -*- vim:set ts=4 sw=4 sts=4 noet: */
#ifndef FLINT_BC_LOCATER_H_
#define FLINT_BC_LOCATER_H_

#include <cctype>
#include <iostream>
#include <map>
#include <string>

#include <boost/noncopyable.hpp>

#include "bc/index.h"

class Locater : boost::noncopyable {
public:
	void SetPosition(const std::string &name, int location) {
		m_.insert(std::make_pair(name, location));
	}

	bool Find(const std::string &name, int *so, int *lo = NULL) const {
		using std::cerr;
		using std::endl;

		switch (name[0]) {
		case '%':
			if (name == "%time") {
				*so = kIndexTime;
				if (!lo) return true;
				*lo = -2;
				return true;
			} else {
				size_t p = name.find('#', 2);
				std::string n;
				if (p == std::string::npos) {
					n = name.substr(1);
				} else {
					n = name.substr(1, p-1);
				}
				std::map<std::string, int>::const_iterator it = m_.find(n);
				if (it == m_.end()) {
					cerr << "missing name: " << n << endl;
					return false;
				}
				*so = it->second;
				if (!lo) return true;
				if (p == std::string::npos) {
					*lo = -1;
				} else {
					const char *s = name.c_str();
					if (!isdigit(s[p+1])) {
						cerr << "invalid suffix: " << name << endl;
						return false;
					}
					int i = atoi(s+p+1);
					*lo = i;
				}
				return true;
			}
			break;
		case '@':
			if (name == "@dt") {
				*so = kIndexDt;
				if (!lo) return true;
				*lo = -2;
				return true;
			}
			break;
		default:
			break;
		}
		cerr << "unknown name: " << name << endl;
		return false;
	}

private:
	std::map<std::string, int> m_;
};

#endif
