/* -*- Mode: C++; tab-width: 4; indent-tabs-mode: t; c-basic-offset: 4 -*- vim:set ts=4 sw=4 sts=4 noet: */
#ifndef FLINT_BC_LOCATER_H_
#define FLINT_BC_LOCATER_H_

#include <cctype>
#include <iostream>
#include <string>
#include <unordered_map>

#include "bc/index.h"

namespace flint {

class Locater {
public:
	Locater(const Locater &) = delete;
	Locater &operator=(const Locater &) = delete;

	Locater() {}

	void SetPosition(const std::string &name, int location) {
		m_.emplace(name, location);
	}

	bool Find(const std::string &name, int *so, int *lo = nullptr) const {
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
				std::unordered_map<std::string, int>::const_iterator it = m_.find(n);
				if (it == m_.end()) {
					std::cerr << "missing name: " << n << std::endl;
					return false;
				}
				*so = it->second;
				if (!lo) return true;
				if (p == std::string::npos) {
					*lo = -1;
				} else {
					const char *s = name.c_str();
					if (!isdigit(s[p+1])) {
						std::cerr << "invalid suffix: " << name << std::endl;
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
		std::cerr << "unknown name: " << name << std::endl;
		return false;
	}

private:
	std::unordered_map<std::string, int> m_;
};

}

#endif
