/* -*- Mode: C++; tab-width: 4; indent-tabs-mode: t; c-basic-offset: 4 -*- vim:set ts=4 sw=4 sts=4 noet: */
#ifndef FLINT_CAS_ICG_H_
#define FLINT_CAS_ICG_H_

#include <memory>

#include <boost/uuid/uuid.hpp>

#include "sqlite3.h"

namespace flint {

namespace db {
class AstInserter;
}

namespace cas {

class Def;
class Ode;
class Printer;

namespace icg {

/*
 * For generating imperative AST for evaluating auxiliary variables of an ODE
 * system.
 */
class AuxVar {
public:
	AuxVar(Printer *printer, sqlite3 *db);

	~AuxVar();

	bool Insert(const boost::uuids::uuid &uuid,
				const Def &def);
	bool Insert(const boost::uuids::uuid &uuid,
				const Ode &ode);

private:
	Printer *printer_;
	std::unique_ptr<db::AstInserter> inserter_;
};

/*
 * For generating imperative AST for evaluating the mass matrix of an ODE
 * system.
 */
class OdeMass {
public:
	OdeMass(Printer *printer, sqlite3 *db);

	~OdeMass();

	bool Insert(const boost::uuids::uuid &uuid,
				const Def &def);
	bool Insert(const boost::uuids::uuid &uuid,
				const Ode &ode);

private:
	Printer *printer_;
	std::unique_ptr<db::AstInserter> inserter_;
};

/*
 * For generate imperative AST for evaluating the right-hand side of an ODE
 * system.
 */
class OdeRhs {
public:
	OdeRhs(Printer *printer, sqlite3 *db);

	~OdeRhs();

	bool Insert(const boost::uuids::uuid &uuid,
				const Def &def);
	bool Insert(const boost::uuids::uuid &uuid,
				const Ode &ode);

private:
	Printer *printer_;
	std::unique_ptr<db::AstInserter> inserter_;
};

}
}
}

#endif
