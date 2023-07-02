/* -*- Mode: C++; tab-width: 4; indent-tabs-mode: t; c-basic-offset: 4 -*- vim:set ts=4 sw=4 sts=4 noet: */
#include "sbml.h"

#include <cassert>
#include <cstdio>
#include <cstdlib>
#include <iostream>
#include <memory>
#include <string>

#include <sbmlsolver/odeModel.h>
#include <sbmlsolver/solverError.h>

#include "db/query.h"
#include "db/utility.h"
#include "modelpath.h"

namespace flint {
namespace {

bool PrintChildrenToOstream(const ASTNode_t *node, std::ostream *os);
bool PrintPiecesToOstream(const ASTNode_t *node, std::ostream *os);

bool PrintToOstream(const ASTNode_t *node, std::ostream *os)
{
	switch (ASTNode_getType(node)) {
	case AST_PLUS:
		*os << " (plus";
		if (!PrintChildrenToOstream(node, os)) return false;
		os->put(')');
		break;
	case AST_MINUS:
		*os << " (minus";
		if (!PrintChildrenToOstream(node, os)) return false;
		os->put(')');
		break;
	case AST_TIMES:
		*os << " (times";
		if (!PrintChildrenToOstream(node, os)) return false;
		os->put(')');
		break;
	case AST_DIVIDE:
		*os << " (divide";
		if (!PrintChildrenToOstream(node, os)) return false;
		os->put(')');
		break;
	case AST_POWER:
		*os << " (power";
		if (!PrintChildrenToOstream(node, os)) return false;
		os->put(')');
		break;
	case AST_INTEGER:
		*os << ' ' << ASTNode_getInteger(node);
		break;
	case AST_REAL:
	case AST_REAL_E:
	case AST_RATIONAL:
		*os << ' ' << ASTNode_getReal(node);
		break;
	case AST_NAME:
		*os << " %sbml:" << ASTNode_getName(node);
		break;
	case AST_NAME_TIME:
		*os << " %time";
		break;
	case AST_FUNCTION:
		std::cerr << "not yet support user-defined functions" << std::endl;
		return false;
	case AST_FUNCTION_ABS:
		*os << " (abs";
		if (!PrintChildrenToOstream(node, os)) return false;
		os->put(')');
		break;
	case AST_FUNCTION_ARCCOS:
		*os << " (arccos";
		if (!PrintChildrenToOstream(node, os)) return false;
		os->put(')');
		break;
	case AST_FUNCTION_ARCCOSH:
		*os << " (arccosh";
		if (!PrintChildrenToOstream(node, os)) return false;
		os->put(')');
		break;
	case AST_FUNCTION_ARCCOT:
		*os << " (arccot";
		if (!PrintChildrenToOstream(node, os)) return false;
		os->put(')');
		break;
	case AST_FUNCTION_ARCCOTH:
		*os << " (arccoth";
		if (!PrintChildrenToOstream(node, os)) return false;
		os->put(')');
		break;
	case AST_FUNCTION_ARCCSC:
		*os << " (arccsc";
		if (!PrintChildrenToOstream(node, os)) return false;
		os->put(')');
		break;
	case AST_FUNCTION_ARCCSCH:
		*os << " (arccsch";
		if (!PrintChildrenToOstream(node, os)) return false;
		os->put(')');
		break;
	case AST_FUNCTION_ARCSEC:
		*os << " (arcsec";
		if (!PrintChildrenToOstream(node, os)) return false;
		os->put(')');
		break;
	case AST_FUNCTION_ARCSECH:
		*os << " (arcsech";
		if (!PrintChildrenToOstream(node, os)) return false;
		os->put(')');
		break;
	case AST_FUNCTION_ARCSIN:
		*os << " (arcsin";
		if (!PrintChildrenToOstream(node, os)) return false;
		os->put(')');
		break;
	case AST_FUNCTION_ARCSINH:
		*os << " (arcsinh";
		if (!PrintChildrenToOstream(node, os)) return false;
		os->put(')');
		break;
	case AST_FUNCTION_ARCTAN:
		*os << " (arctan";
		if (!PrintChildrenToOstream(node, os)) return false;
		os->put(')');
		break;
	case AST_FUNCTION_ARCTANH:
		*os << " (arctanh";
		if (!PrintChildrenToOstream(node, os)) return false;
		os->put(')');
		break;
	case AST_FUNCTION_CEILING:
		*os << " (ceil";
		if (!PrintChildrenToOstream(node, os)) return false;
		os->put(')');
		break;
	case AST_FUNCTION_COS:
		*os << " (cos";
		if (!PrintChildrenToOstream(node, os)) return false;
		os->put(')');
		break;
	case AST_FUNCTION_COSH:
		*os << " (cosh";
		if (!PrintChildrenToOstream(node, os)) return false;
		os->put(')');
		break;
	case AST_FUNCTION_COT:
		*os << " (cot";
		if (!PrintChildrenToOstream(node, os)) return false;
		os->put(')');
		break;
	case AST_FUNCTION_COTH:
		*os << " (coth";
		if (!PrintChildrenToOstream(node, os)) return false;
		os->put(')');
		break;
	case AST_FUNCTION_CSC:
		*os << " (csc";
		if (!PrintChildrenToOstream(node, os)) return false;
		os->put(')');
		break;
	case AST_FUNCTION_CSCH:
		*os << " (csch";
		if (!PrintChildrenToOstream(node, os)) return false;
		os->put(')');
		break;
	case AST_FUNCTION_DELAY:
		std::cerr << "not yet support DELAY" << std::endl;
		return false;
	case AST_FUNCTION_EXP:
		*os << " (exp";
		if (!PrintChildrenToOstream(node, os)) return false;
		os->put(')');
		break;
	case AST_FUNCTION_FACTORIAL:
		*os << " (factorial";
		if (!PrintChildrenToOstream(node, os)) return false;
		os->put(')');
		break;
	case AST_FUNCTION_FLOOR:
		*os << " (floor";
		if (!PrintChildrenToOstream(node, os)) return false;
		os->put(')');
		break;
	case AST_FUNCTION_LN:
		*os << " (ln";
		if (!PrintChildrenToOstream(node, os)) return false;
		os->put(')');
		break;
	case AST_FUNCTION_LOG:
		*os << " (log";
		if (!PrintChildrenToOstream(node, os)) return false;
		os->put(')');
		break;
	case AST_FUNCTION_PIECEWISE:
		*os << " (piecewise";
		if (!PrintPiecesToOstream(node, os)) return false;
		os->put(')');
		break;
	case AST_FUNCTION_POWER:
		*os << " (power";
		if (!PrintChildrenToOstream(node, os)) return false;
		os->put(')');
		break;
	case AST_FUNCTION_ROOT:
		*os << " (root";
		if (!PrintChildrenToOstream(node, os)) return false;
		os->put(')');
		break;
	case AST_FUNCTION_SEC:
		*os << " (sec";
		if (!PrintChildrenToOstream(node, os)) return false;
		os->put(')');
		break;
	case AST_FUNCTION_SECH:
		*os << " (sech";
		if (!PrintChildrenToOstream(node, os)) return false;
		os->put(')');
		break;
	case AST_FUNCTION_SIN:
		*os << " (sin";
		if (!PrintChildrenToOstream(node, os)) return false;
		os->put(')');
		break;
	case AST_FUNCTION_SINH:
		*os << " (sinh";
		if (!PrintChildrenToOstream(node, os)) return false;
		os->put(')');
		break;
	case AST_FUNCTION_TAN:
		*os << " (tan";
		if (!PrintChildrenToOstream(node, os)) return false;
		os->put(')');
		break;
	case AST_FUNCTION_TANH:
		*os << " (tanh";
		if (!PrintChildrenToOstream(node, os)) return false;
		os->put(')');
		break;
	case AST_RELATIONAL_EQ:
		*os << " (eq";
		if (!PrintChildrenToOstream(node, os)) return false;
		os->put(')');
		break;
	case AST_RELATIONAL_GEQ:
		*os << " (geq";
		if (!PrintChildrenToOstream(node, os)) return false;
		os->put(')');
		break;
	case AST_RELATIONAL_GT:
		*os << " (gt";
		if (!PrintChildrenToOstream(node, os)) return false;
		os->put(')');
		break;
	case AST_RELATIONAL_LEQ:
		*os << " (leq";
		if (!PrintChildrenToOstream(node, os)) return false;
		os->put(')');
		break;
	case AST_RELATIONAL_LT:
		*os << " (lt";
		if (!PrintChildrenToOstream(node, os)) return false;
		os->put(')');
		break;
	case AST_RELATIONAL_NEQ:
		*os << " (neq";
		if (!PrintChildrenToOstream(node, os)) return false;
		os->put(')');
		break;
	case AST_LOGICAL_AND:
		*os << " (and";
		if (!PrintChildrenToOstream(node, os)) return false;
		os->put(')');
		break;
	case AST_LOGICAL_NOT:
		*os << " (not";
		if (!PrintChildrenToOstream(node, os)) return false;
		os->put(')');
		break;
	case AST_LOGICAL_OR:
		*os << " (or";
		if (!PrintChildrenToOstream(node, os)) return false;
		os->put(')');
		break;
	case AST_LOGICAL_XOR:
		*os << " (xor";
		if (!PrintChildrenToOstream(node, os)) return false;
		os->put(')');
		break;
	default:
		if (ASTNode_isBoolean(node)) {
			std::cerr << "not yet support Boolean" << std::endl;
			return false;
		} else if (ASTNode_isConstant(node)) { // true, false, pi, exponentiale
			switch (ASTNode_getType(node)) {
			case AST_CONSTANT_E:
				*os << " exponentiale";
				break;
			case AST_CONSTANT_FALSE:
				*os << " false";
				break;
			case AST_CONSTANT_PI:
				*os << " pi";
				break;
			case AST_CONSTANT_TRUE:
				*os << " true";
				break;
			default:
				std::cerr << "unknown Constant: " << ASTNode_getType(node) << std::endl;
				return false;
			}
		} else if (ASTNode_isInfinity(node)) {
			std::cerr << "not yet support Infinity" << std::endl;
			return false;
		} else if (ASTNode_isLambda(node)) {
			std::cerr << "not yet support Lambda" << std::endl;
			return false;
		} else if (ASTNode_isNaN(node)) {
			std::cerr << "not yet support NaN" << std::endl;
			return false;
		} else if (ASTNode_isNegInfinity(node)) {
			std::cerr << "not yet support NegInfinity" << std::endl;
			return false;
		} else if (ASTNode_isPiecewise(node)) {
			assert(false); // this shoud not happen
		} else {
			std::cerr << "unknown node" << std::endl;
			return false;
		}
	}
	return true;
}

bool PrintChildrenToOstream(const ASTNode_t *node, std::ostream *os)
{
	int num = ASTNode_getNumChildren(node);
	for (int i=0; i<num; i++) {
		if (!PrintToOstream(ASTNode_getChild(node, i), os)) return false;
	}
	return true;
}

bool PrintPiecesToOstream(const ASTNode_t *node, std::ostream *os)
{
	int num = ASTNode_getNumChildren(node);
	if (num <= 0) {
		std::cerr << "neither <piece> nor <otherwise> in <piecewise>" << std::endl;
		return false;
	}
	int i = 0;
	while (i < num-1) {
		*os << " (piece";
		PrintToOstream(ASTNode_getChild(node, i), os);
		PrintToOstream(ASTNode_getChild(node, i+1), os);
		os->put(')');
		i += 2;
	}
	*os << " (otherwise";
	PrintToOstream(ASTNode_getChild(node, num-1), os);
	os->put(')');
	return true;
}

class Analysis {
public:
	Analysis(sqlite3 *db, odeModel_t *model)
		: db_(db)
		, model_(model)
		, stmt_a_(nullptr)
		, stmt_c_(nullptr)
		, stmt_o_(nullptr)
	{
		assert(model);
	}

	~Analysis() {
		sqlite3_finalize(stmt_a_);
		sqlite3_finalize(stmt_c_);
		sqlite3_finalize(stmt_o_);
		ODEModel_free(model_);
	}

	bool Run() {
		if ( !CreateTable(db_, "assignments", "(name TEXT, math TEXT)") ||
			 !CreateTable(db_, "constants", "(name TEXT, value REAL)") ||
			 !CreateTable(db_, "odes", "(name TEXT, initial_value REAL, math TEXT)") )
			return false;

		int e;
		e = db::PrepareStatement(db_, "INSERT INTO assignments VALUES (?, ?)", &stmt_a_);
		if (e != SQLITE_OK) {
			std::cerr << "failed to prepare statement: " << e << std::endl;
			return false;
		}
		e = db::PrepareStatement(db_, "INSERT INTO constants VALUES (?, ?)", &stmt_c_);
		if (e != SQLITE_OK) {
			std::cerr << "failed to prepare statement: " << e << std::endl;
			return false;
		}
		e = db::PrepareStatement(db_, "INSERT INTO odes VALUES (?, ?, ?)", &stmt_o_);
		if (e != SQLITE_OK) {
			std::cerr << "failed to prepare statement: " << e << std::endl;
			return false;
		}

		if (!BeginTransaction(db_)) return false;

		std::unique_ptr<char[]> sbml_name;
		int neq = ODEModel_getNeq(model_);
		int nass = ODEModel_getNumAssignments(model_);
		int nconst = ODEModel_getNumConstants(model_);
		for (int i=0; i<neq; i++) {
			variableIndex_t *vi = ODEModel_getOdeVariableIndex(model_, i);
			if (!vi) break;
			const ASTNode_t *node = ODEModel_getOde(model_, vi);
			if (!node) {
				VariableIndex_free(vi);
				break;
			}
			const char *name = VariableIndex_getName(vi, model_);
			sbml_name.reset(new char[6+std::strlen(name)]);
			sprintf(sbml_name.get(), "sbml:%s", name);
			VariableIndex_free(vi);
			std::ostringstream oss;
			if (!PrintToOstream(node, &oss)) return false;
			std::string math = oss.str();
			if (!InsertOde(sbml_name.get(), model_->values[i], math.c_str()))
				return false;
		}
		for (int i=0; i<nass; i++) {
			variableIndex_t *vi = ODEModel_getAssignedVariableIndex(model_, i);
			if (!vi) break;
			const ASTNode_t *node = ODEModel_getAssignment(model_, vi);
			if (!node) {
				VariableIndex_free(vi);
				break;
			}
			const char *name = VariableIndex_getName(vi, model_);
			sbml_name.reset(new char[6+std::strlen(name)]);
			sprintf(sbml_name.get(), "sbml:%s", name);
			VariableIndex_free(vi);
			std::ostringstream oss;
			if (!PrintToOstream(node, &oss)) return false;
			std::string math = oss.str();
			if (!InsertAssignment(sbml_name.get(), math.c_str()))
				return false;
		}
		for (int i=0; i<nconst; i++) {
			variableIndex_t *vi = ODEModel_getConstantIndex(model_, i);
			if (!vi) break;
			const char *name = VariableIndex_getName(vi, model_);
			sbml_name.reset(new char[6+std::strlen(name)]);
			sprintf(sbml_name.get(), "sbml:%s", name);
			VariableIndex_free(vi);
			if (!InsertConstant(sbml_name.get(), model_->values[neq+nass+i]))
				return false;
		}
		return CommitTransaction(db_);
	}

private:
	bool InsertAssignment(const char *name, const char *math)
	{
		int e;
		e = sqlite3_bind_text(stmt_a_, 1, name, -1, SQLITE_STATIC);
		if (e != SQLITE_OK) {
			std::cerr << "failed to bind name: " << e << std::endl;
			return false;
		}
		e = sqlite3_bind_text(stmt_a_, 2, math, -1, SQLITE_STATIC);
		if (e != SQLITE_OK) {
			std::cerr << "failed to bind math: " << e << std::endl;
			return false;
		}
		e = sqlite3_step(stmt_a_);
		if (e != SQLITE_DONE) {
			std::cerr << "failed to step statement: " << e << std::endl;
			return false;
		}
		sqlite3_reset(stmt_a_);
		return true;
	}

	bool InsertConstant(const char *name, double value)
	{
		int e;
		e = sqlite3_bind_text(stmt_c_, 1, name, -1, SQLITE_STATIC);
		if (e != SQLITE_OK) {
			std::cerr << "failed to bind name: " << e << std::endl;
			return false;
		}
		e = sqlite3_bind_double(stmt_c_, 2, value);
		if (e != SQLITE_OK) {
			std::cerr << "failed to bind value: " << e << std::endl;
			return false;
		}
		e = sqlite3_step(stmt_c_);
		if (e != SQLITE_DONE) {
			std::cerr << "failed to step statement: " << e << std::endl;
			return false;
		}
		sqlite3_reset(stmt_c_);
		return true;
	}

	bool InsertOde(const char *name, double initial_value, const char *math)
	{
		int e;
		e = sqlite3_bind_text(stmt_o_, 1, name, -1, SQLITE_STATIC);
		if (e != SQLITE_OK) {
			std::cerr << "failed to bind name: " << e << std::endl;
			return false;
		}
		e = sqlite3_bind_double(stmt_o_, 2, initial_value);
		if (e != SQLITE_OK) {
			std::cerr << "failed to bind initial_value: " << e << std::endl;
			return false;
		}
		e = sqlite3_bind_text(stmt_o_, 3, math, -1, SQLITE_STATIC);
		if (e != SQLITE_OK) {
			std::cerr << "failed to bind math: " << e << std::endl;
			return false;
		}
		e = sqlite3_step(stmt_o_);
		if (e != SQLITE_DONE) {
			std::cerr << "failed to step statement: " << e << std::endl;
			return false;
		}
		sqlite3_reset(stmt_o_);
		return true;
	}

	sqlite3 *db_;
	odeModel_t *model_;
	sqlite3_stmt *stmt_a_;
	sqlite3_stmt *stmt_c_;
	sqlite3_stmt *stmt_o_;
};

} // namespace

namespace sbml {

bool Parse(sqlite3 *db)
{
	std::unique_ptr<char[]> model_file(GetModelFilename(db));
	if (!model_file)
		return false;
	odeModel_t *model = ODEModel_createFromFile(model_file.get());
	if (!model) {
		std::cerr << "could not create ODE system from an SBML model: " << model_file.get() << std::endl;
		SolverError_dumpAndClearErrors();
		return false;
	}
	std::unique_ptr<Analysis> analysis(new Analysis(db, model));
	return analysis->Run();
}

}
}
