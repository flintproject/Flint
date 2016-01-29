/* -*- Mode: C++; tab-width: 4; indent-tabs-mode: t; c-basic-offset: 4 -*- vim:set ts=4 sw=4 sts=4 noet: */
#ifndef FLINT_CAS_PRINTER_H_
#define FLINT_CAS_PRINTER_H_

#include <memory>
#include <sstream>
#include <string>

#include <boost/uuid/uuid.hpp>

#include "cas.h"
#include "sqlite3.h"

namespace flint {

class VariableMap;

namespace cas {

class Printer : public boost::static_visitor<> {
public:
	Printer();

	~Printer();

	bool Load(sqlite3 *db);

	void set_uuid(const boost::uuids::uuid &uuid);

	void operator()(const Compound &c);

	void operator()(const Identifier &s);

	void operator()(int i);

	void operator()(const flint::lexer::Real &r);

	std::string GetAndClearString();

private:
	std::unique_ptr<VariableMap> vm_;
	boost::uuids::uuid uuid_;
	std::ostringstream os_;
};

}
}

#endif
