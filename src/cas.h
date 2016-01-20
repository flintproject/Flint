/* -*- Mode: C++; tab-width: 4; indent-tabs-mode: t; c-basic-offset: 4 -*- vim:set ts=4 sw=4 sts=4 noet: */
#ifndef FLINT_CAS_H_
#define FLINT_CAS_H_

#include <map>
#include <memory>
#include <set>
#include <utility>
#include <vector>

#include <boost/fusion/include/adapt_struct.hpp>
#include <boost/variant/recursive_variant.hpp>
#include <boost/uuid/uuid.hpp>

#include "lexer.hh"
#include "sqlite3.h"

namespace flint {
namespace cas {

struct Compound;

enum {
	kExprIsCompound,
	kExprIsString,
	kExprIsInteger,
	kExprIsReal
};

typedef boost::variant<boost::recursive_wrapper<Compound>,
					   std::string,
					   int,
					   lexer::Real
					   > Expr;

struct Compound {
	std::string keyword;
	std::vector<Expr> children;
	int col;
	int row;
};

}
}

BOOST_FUSION_ADAPT_STRUCT(flint::cas::Compound,
						  (std::string, keyword)
						  (std::vector<flint::cas::Expr>, children)
						  (int, col)
						  (int, row)
						  )

namespace flint {
namespace cas {

class Eq {
public:
	virtual ~Eq();

	const std::string &name() const;
	int col() const;
	int row() const;
	const Expr &rhs() const;

protected:
	Eq(const std::string &name, int col, int row, const Expr &rhs);

	std::string name_;
	int col_;
	int row_;
	Expr rhs_;
};

class Ode : public Eq {
public:
	Ode(const std::string &name, int col, int row, const Expr &rhs, const Expr &mass);

	Expr &mass();
	const Expr &mass() const;

private:
	Expr mass_;
};

class Def : public Eq {
public:
	Def(const std::string &name, int col, int row, const Expr &rhs);

private:
};

class Printer;

class System {
public:
	System();

	~System();

	/*
	 * Note that db is for read only.
	 */
	bool Load(sqlite3 *db);

	void Add(const boost::uuids::uuid &uuid, Ode ode);

	void Add(const boost::uuids::uuid &uuid, Def def);

	bool SaveAuxVarBc(const char *file);

	bool SaveOdeMassBc(const char *file);

	bool SaveOdeRhsBc(const char *file);

	/*
	 * Return true in case of success, false otherwise.
	 * Output at 3rd argument is an empty string if the mass matrix is identity.
	 */
	bool FindMass(const boost::uuids::uuid &uuid, const std::string &name,
				  std::string *found) const;

private:
	bool GenerateAuxVarAst(sqlite3 *db);

	bool GenerateOdeMassAst(sqlite3 *db);

	bool GenerateOdeRhsAst(sqlite3 *db);

	typedef std::map<boost::uuids::uuid, std::vector<Ode> > OdeMap;
	typedef std::map<boost::uuids::uuid, std::vector<Def> > DefMap;

	std::unique_ptr<Printer> printer_;
	std::set<boost::uuids::uuid> uuids_;
	OdeMap odes_;
	DefMap defs_;
};

/*
 * Annotate declarative equations.
 * Note that db is for read only.
 * Return true in case of success, otherwise false.
 */
bool AnnotateEquations(sqlite3 *db, const char *input, System *output);

}
}

#endif
