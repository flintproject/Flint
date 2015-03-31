/* -*- Mode: C++; tab-width: 4; indent-tabs-mode: t; c-basic-offset: 4 -*- vim:set ts=4 sw=4 sts=4 noet: */
#include <cassert>
#include <cstdio>
#include <cstdlib>
#include <iostream>
#include <string>

#include <boost/program_options.hpp>
#include <boost/scoped_array.hpp>
#include <boost/scoped_ptr.hpp>

#include <sbmlsolver/odeModel.h>
#include <sbmlsolver/solverError.h>

#include "modelpath.h"

namespace po = boost::program_options;

using std::cerr;
using std::endl;
using std::string;
using std::printf;
using std::putchar;

namespace {

class Analysis {
public:
	explicit Analysis(odeModel_t *model) : model_(model) {
		assert(model);
	}
	~Analysis() {
		ODEModel_free(model_);
	}

	bool Run() {
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
			printf("o sbml:%s", VariableIndex_getName(vi, model_));
			VariableIndex_free(vi);
			printf(" %g", model_->values[i]);
			if (!Print(node)) return false;
			putchar('\n');
		}
		for (int i=0; i<nass; i++) {
			variableIndex_t *vi = ODEModel_getAssignedVariableIndex(model_, i);
			if (!vi) break;
			const ASTNode_t *node = ODEModel_getAssignment(model_, vi);
			if (!node) {
				VariableIndex_free(vi);
				break;
			}
			printf("a sbml:%s", VariableIndex_getName(vi, model_));
			VariableIndex_free(vi);
			if (!Print(node)) return false;
			putchar('\n');
		}
		for (int i=0; i<nconst; i++) {
			variableIndex_t *vi = ODEModel_getConstantIndex(model_, i);
			if (!vi) break;
			printf("c sbml:%s", VariableIndex_getName(vi, model_));
			VariableIndex_free(vi);
			printf(" %g\n", model_->values[neq+nass+i]);
		}
		return true;
	}

private:
	bool Print(const ASTNode_t *node) const {
		switch (ASTNode_getType(node)) {
		case AST_PLUS:
			printf(" (plus");
			if (!PrintChildren(node)) return false;
			putchar(')');
			break;
		case AST_MINUS:
			printf(" (minus");
			if (!PrintChildren(node)) return false;
			putchar(')');
			break;
		case AST_TIMES:
			printf(" (times");
			if (!PrintChildren(node)) return false;
			putchar(')');
			break;
		case AST_DIVIDE:
			printf(" (divide");
			if (!PrintChildren(node)) return false;
			putchar(')');
			break;
		case AST_POWER:
			printf(" (power");
			if (!PrintChildren(node)) return false;
			putchar(')');
			break;
		case AST_INTEGER:
			printf(" %ld", ASTNode_getInteger(node));
			break;
		case AST_REAL:
		case AST_REAL_E:
		case AST_RATIONAL:
			printf(" %g", ASTNode_getReal(node));
			break;
		case AST_NAME:
			printf(" %%sbml:%s", ASTNode_getName(node));
			break;
		case AST_NAME_TIME:
			printf(" %%time");
			break;
		case AST_FUNCTION:
			cerr << "not yet support user-defined functions" << endl;
			return false;
		case AST_FUNCTION_ABS:
			printf(" (abs");
			if (!PrintChildren(node)) return false;
			putchar(')');
			break;
		case AST_FUNCTION_ARCCOS:
			printf(" (arccos");
			if (!PrintChildren(node)) return false;
			putchar(')');
			break;
		case AST_FUNCTION_ARCCOSH:
			printf(" (arccosh");
			if (!PrintChildren(node)) return false;
			putchar(')');
			break;
		case AST_FUNCTION_ARCCOT:
			printf(" (arccot");
			if (!PrintChildren(node)) return false;
			putchar(')');
			break;
		case AST_FUNCTION_ARCCOTH:
			printf(" (arccoth");
			if (!PrintChildren(node)) return false;
			putchar(')');
			break;
		case AST_FUNCTION_ARCCSC:
			printf(" (arccsc");
			if (!PrintChildren(node)) return false;
			putchar(')');
			break;
		case AST_FUNCTION_ARCCSCH:
			printf(" (arccsch");
			if (!PrintChildren(node)) return false;
			putchar(')');
			break;
		case AST_FUNCTION_ARCSEC:
			printf(" (arcsec");
			if (!PrintChildren(node)) return false;
			putchar(')');
			break;
		case AST_FUNCTION_ARCSECH:
			printf(" (arcsech");
			if (!PrintChildren(node)) return false;
			putchar(')');
			break;
		case AST_FUNCTION_ARCSIN:
			printf(" (arcsin");
			if (!PrintChildren(node)) return false;
			putchar(')');
			break;
		case AST_FUNCTION_ARCSINH:
			printf(" (arcsinh");
			if (!PrintChildren(node)) return false;
			putchar(')');
			break;
		case AST_FUNCTION_ARCTAN:
			printf(" (arctan");
			if (!PrintChildren(node)) return false;
			putchar(')');
			break;
		case AST_FUNCTION_ARCTANH:
			printf(" (arctanh");
			if (!PrintChildren(node)) return false;
			putchar(')');
			break;
		case AST_FUNCTION_CEILING:
			printf(" (ceil");
			if (!PrintChildren(node)) return false;
			putchar(')');
			break;
		case AST_FUNCTION_COS:
			printf(" (cos");
			if (!PrintChildren(node)) return false;
			putchar(')');
			break;
		case AST_FUNCTION_COSH:
			printf(" (cosh");
			if (!PrintChildren(node)) return false;
			putchar(')');
			break;
		case AST_FUNCTION_COT:
			printf(" (cot");
			if (!PrintChildren(node)) return false;
			putchar(')');
			break;
		case AST_FUNCTION_COTH:
			printf(" (coth");
			if (!PrintChildren(node)) return false;
			putchar(')');
			break;
		case AST_FUNCTION_CSC:
			printf(" (csc");
			if (!PrintChildren(node)) return false;
			putchar(')');
			break;
		case AST_FUNCTION_CSCH:
			printf(" (csch");
			if (!PrintChildren(node)) return false;
			putchar(')');
			break;
		case AST_FUNCTION_DELAY:
			cerr << "not yet support DELAY" << endl;
			return false;
		case AST_FUNCTION_EXP:
			printf(" (exp");
			if (!PrintChildren(node)) return false;
			putchar(')');
			break;
		case AST_FUNCTION_FACTORIAL:
			printf(" (factorial");
			if (!PrintChildren(node)) return false;
			putchar(')');
			break;
		case AST_FUNCTION_FLOOR:
			printf(" (floor");
			if (!PrintChildren(node)) return false;
			putchar(')');
			break;
		case AST_FUNCTION_LN:
			printf(" (ln");
			if (!PrintChildren(node)) return false;
			putchar(')');
			break;
		case AST_FUNCTION_LOG:
			printf(" (log");
			if (!PrintChildren(node)) return false;
			putchar(')');
			break;
		case AST_FUNCTION_PIECEWISE:
			printf(" (piecewise");
			if (!PrintPieces(node)) return false;
			putchar(')');
			break;
		case AST_FUNCTION_POWER:
			printf(" (power");
			if (!PrintChildren(node)) return false;
			putchar(')');
			break;
		case AST_FUNCTION_ROOT:
			printf(" (root");
			if (!PrintChildren(node)) return false;
			putchar(')');
			break;
		case AST_FUNCTION_SEC:
			printf(" (sec");
			if (!PrintChildren(node)) return false;
			putchar(')');
			break;
		case AST_FUNCTION_SECH:
			printf(" (sech");
			if (!PrintChildren(node)) return false;
			putchar(')');
			break;
		case AST_FUNCTION_SIN:
			printf(" (sin");
			if (!PrintChildren(node)) return false;
			putchar(')');
			break;
		case AST_FUNCTION_SINH:
			printf(" (sinh");
			if (!PrintChildren(node)) return false;
			putchar(')');
			break;
		case AST_FUNCTION_TAN:
			printf(" (tan");
			if (!PrintChildren(node)) return false;
			putchar(')');
			break;
		case AST_FUNCTION_TANH:
			printf(" (tanh");
			if (!PrintChildren(node)) return false;
			putchar(')');
			break;
		case AST_RELATIONAL_EQ:
			printf(" (eq");
			if (!PrintChildren(node)) return false;
			putchar(')');
			break;
		case AST_RELATIONAL_GEQ:
			printf(" (geq");
			if (!PrintChildren(node)) return false;
			putchar(')');
			break;
		case AST_RELATIONAL_GT:
			printf(" (gt");
			if (!PrintChildren(node)) return false;
			putchar(')');
			break;
		case AST_RELATIONAL_LEQ:
			printf(" (leq");
			if (!PrintChildren(node)) return false;
			putchar(')');
			break;
		case AST_RELATIONAL_LT:
			printf(" (lt");
			if (!PrintChildren(node)) return false;
			putchar(')');
			break;
		case AST_RELATIONAL_NEQ:
			printf(" (neq");
			if (!PrintChildren(node)) return false;
			putchar(')');
			break;
		case AST_LOGICAL_AND:
			printf(" (and");
			if (!PrintChildren(node)) return false;
			putchar(')');
			break;
		case AST_LOGICAL_NOT:
			printf(" (not");
			if (!PrintChildren(node)) return false;
			putchar(')');
			break;
		case AST_LOGICAL_OR:
			printf(" (or");
			if (!PrintChildren(node)) return false;
			putchar(')');
			break;
		case AST_LOGICAL_XOR:
			printf(" (xor");
			if (!PrintChildren(node)) return false;
			putchar(')');
			break;
		default:
			if (ASTNode_isBoolean(node)) {
				cerr << "not yet support Boolean" << endl;
				return false;
			} else if (ASTNode_isConstant(node)) { // true, false, pi, exponentiale
				switch (ASTNode_getType(node)) {
				case AST_CONSTANT_E:
					printf(" exponentiale");
					break;
				case AST_CONSTANT_FALSE:
					printf(" false");
					break;
				case AST_CONSTANT_PI:
					printf(" pi");
					break;
				case AST_CONSTANT_TRUE:
					printf(" true");
					break;
				default:
					cerr << "unknown Constant: " << ASTNode_getType(node) << endl;
					return false;
				}
			} else if (ASTNode_isInfinity(node)) {
				cerr << "not yet support Infinity" << endl;
				return false;
			} else if (ASTNode_isLambda(node)) {
				cerr << "not yet support Lambda" << endl;
				return false;
			} else if (ASTNode_isNaN(node)) {
				cerr << "not yet support NaN" << endl;
				return false;
			} else if (ASTNode_isNegInfinity(node)) {
				cerr << "not yet support NegInfinity" << endl;
				return false;
			} else if (ASTNode_isPiecewise(node)) {
				assert(false); // this shoud not happen
			} else {
				cerr << "unknown node" << endl;
				return false;
			}
		}
		return true;
	}

	bool PrintChildren(const ASTNode_t *node) const {
		int num = ASTNode_getNumChildren(node);
		for (int i=0; i<num; i++) {
			if (!Print(ASTNode_getChild(node, i))) return false;
		}
		return true;
	}

	bool PrintPieces(const ASTNode_t *node) const {
		int num = ASTNode_getNumChildren(node);
		if (num <= 0) {
			cerr << "neither <piece> nor <otherwise> in <piecewise>" << endl;
			return false;
		}
		int i = 0;
		while (i < num-1) {
			printf(" (piece");
			Print(ASTNode_getChild(node, i));
			Print(ASTNode_getChild(node, i+1));
			putchar(')');
			i += 2;
		}
		printf(" (otherwise");
		Print(ASTNode_getChild(node, num-1));
		putchar(')');
		return true;
	}

	odeModel_t *model_;
};

} // namespace

int main(int argc, char *argv[])
{
	po::options_description opts("options");
	po::positional_options_description popts;
	po::variables_map vm;
	string input_file;
	int print_help = 0;

	opts.add_options()
		("input", po::value<string>(&input_file), "Input file name")
		("help,h", "Show this message");
	popts.add("input", 1);

	try {
		po::store(po::command_line_parser(argc, argv).options(opts).positional(popts).run(), vm);
		po::notify(vm);
		if (vm.count("help") > 0) print_help = 1;
		if (vm.count("input") == 0) print_help = 2;
	} catch (const po::error &) {
		print_help = 2;
	}
	if (print_help) {
		cerr << "usage: " << argv[0] << " PATH" << endl;
		cerr << opts;
		return (print_help == 1) ? EXIT_SUCCESS : EXIT_FAILURE;
	}

	boost::scoped_array<char> model_file(GetModelFilename(input_file.c_str()));
	odeModel_t *model = ODEModel_createFromFile(model_file.get());
	if (!model) {
		cerr << "could not create ODE system from an SBML model: " << model_file.get() << endl;
		SolverError_dumpAndClearErrors();
		return EXIT_FAILURE;
	}
	boost::scoped_ptr<Analysis> analysis(new Analysis(model));
	if (!analysis->Run()) return EXIT_FAILURE;

	return EXIT_SUCCESS;
}
