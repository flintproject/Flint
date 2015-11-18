/* -*- Mode: C++; tab-width: 4; indent-tabs-mode: t; c-basic-offset: 4 -*- vim:set ts=4 sw=4 sts=4 noet: */
#ifndef FLINT_NAME_H_
#define FLINT_NAME_H_

#include <string>

namespace flint {

class Name {
public:
	Name(const Name &) = delete;
	Name &operator=(const Name &) = delete;

	Name(char type, int id, const char *name, const char *unit, double capacity);

	char type() const {return type_;}
	int id() const {return id_;}
	const std::string &name() const {return name_;}
	const std::string &unit() const {return unit_;}
	double capacity() const {return capacity_;}

private:
	char type_;
	int id_;
	std::string name_;
	std::string unit_;
	double capacity_;
};

}

#endif
