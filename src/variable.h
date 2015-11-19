/* -*- Mode: C++; tab-width: 4; indent-tabs-mode: t; c-basic-offset: 4 -*- vim:set ts=4 sw=4 sts=4 noet: */
#ifndef FLINT_NAME_H_
#define FLINT_NAME_H_

#include <string>

namespace flint {

class Variable {
public:
	Variable(const Variable &) = delete;
	Variable &operator=(const Variable &) = delete;

	Variable(char type, int id, const char *name, const char *unit, int col, int row, double capacity);

	char type() const {return type_;}
	int id() const {return id_;}
	const std::string &name() const {return name_;}
	const std::string &unit() const {return unit_;}
	int col() const {return col_;}
	int row() const {return row_;}
	double capacity() const {return capacity_;}

private:
	char type_;
	int id_;
	std::string name_;
	std::string unit_;
	int col_;
	int row_;
	double capacity_;
};

}

#endif
