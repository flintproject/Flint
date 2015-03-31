%{
/* Prologue */
#ifdef HAVE_CONFIG_H
#include "config.h"
#endif

#include <cmath>
#include <cstdio>
#include <cstdlib>
#include <cstring>
#include <fstream>
#include <iostream>
#include <map>

#include <boost/noncopyable.hpp>
#include <boost/program_options.hpp>
#include <boost/scoped_array.hpp>
#include <boost/scoped_ptr.hpp>
#include <boost/uuid/uuid_generators.hpp>
#include <boost/uuid/uuid_io.hpp>

#include "db/bridge-loader.h"
#include "db/driver.h"
#include "db/name_loader.h"
#include "cas/sexp.h"

namespace po = boost::program_options;

using std::atoi;
using std::cerr;
using std::cin;
using std::endl;
using std::ifstream;
using std::ofstream;
using std::ios;
using std::make_pair;
using std::map;
using std::string;
using std::sscanf;
using std::strlen;
using std::strcmp;

extern FILE *yyin;
int yylex();
void yyerror(char const *);

namespace {

typedef map<string, string> BridgeMap;

BridgeMap *GetBridgeMap()
{
	static boost::scoped_ptr<BridgeMap> bm(new BridgeMap);
	return bm.get();
}

class LineWriter : boost::noncopyable {
public:
	LineWriter() : uuid_(NULL), ofs_() {}

    ~LineWriter() {
		ofs_.close();
	}

	void set_uuid(const char *uuid) {uuid_ = uuid;}

	bool OpenFile(const string &file) {
		ofs_.open(file.c_str(), ios::out|ios::app|ios::binary);
		return ofs_.is_open();
	}

protected:
	const char *uuid_;
	ofstream ofs_;
};

class NameWriter : public LineWriter {
public:
	NameWriter() : LineWriter(), pq_id_() {}

	void set_pq_id(int pq_id) {pq_id_ = pq_id;}

	void Write(char c, const char *name) {
		ofs_ << uuid_ << " " << c << " " << ++pq_id_ << " " << name << endl;
	}

private:
	int pq_id_;
};

NameWriter *GetNameWriter()
{
	static boost::scoped_ptr<NameWriter> writer(new NameWriter);
	return writer.get();
}

class ValueWriter : public LineWriter {
public:
	ValueWriter() : LineWriter() {}

	template<typename TNumber>
	void Write(const char *name, TNumber x) {
		ofs_ << uuid_ << " " << "(eq %" << name << " " << x << ")" << endl;
	}
};

ValueWriter *GetValueWriter()
{
	static boost::scoped_ptr<ValueWriter> writer(new ValueWriter);
	return writer.get();
}

class FunctionWriter : public LineWriter {
public:
	FunctionWriter() : LineWriter() {}

	void WriteFunction(const char *name, Sexp *sexp) {
		ofs_ << uuid_ << " (eq %" << name << " ";
		sexp->Write(ofs_);
		ofs_ << ")" << endl;
	}

	void WriteSet(const char *name, const string &pq_name) {
		ofs_ << uuid_ << " (eq %" << name << " %" << pq_name << ")" << endl;
	}

	void WriteGet(const string &pq_name, const char *name) {
		ofs_ << uuid_ << " (eq %" << pq_name << " %sbml:" << name << ")" << endl;
	}
};

FunctionWriter *GetFunctionWriter()
{
	static boost::scoped_ptr<FunctionWriter> writer(new FunctionWriter);
	return writer.get();
}

class OdeWriter : public LineWriter {
public:
	OdeWriter() : LineWriter() {}

	void WriteOde(const char *name, Sexp *sexp) {
		ofs_ << uuid_ << " " << "(eq (diff (bvar %time) %" << name << ") ";
		sexp->Write(ofs_);
		ofs_ << ")" << endl;
	}
};

OdeWriter *GetOdeWriter()
{
	static boost::scoped_ptr<OdeWriter> writer(new OdeWriter);
	return writer.get();
}

template<typename TNumber>
void AddOde(const char *name, TNumber x, Sexp *sexp)
{
	const BridgeMap *bm = GetBridgeMap();
	BridgeMap::const_iterator it = bm->find(name);
	if (it == bm->end()) {
		GetNameWriter()->Write('x', name);
		GetValueWriter()->Write(name, x);
		GetOdeWriter()->WriteOde(name, sexp);
	} else {
		GetNameWriter()->Write('v', name);
		GetFunctionWriter()->WriteSet(name, it->second);
	}
}

void AddAssignment(const char *name, Sexp *sexp)
{
	const BridgeMap *bm = GetBridgeMap();
	BridgeMap::const_iterator it = bm->find(name);
	if (it == bm->end()) {
		GetNameWriter()->Write('v', name);
		GetFunctionWriter()->WriteFunction(name, sexp);
	} else {
		GetNameWriter()->Write('v', name);
		GetFunctionWriter()->WriteSet(name, it->second);
	}
}

template<typename TNumber>
void AddConstant(const char *name, TNumber x)
{
	const BridgeMap *bm = GetBridgeMap();
	BridgeMap::const_iterator it = bm->find(name);
	if (it == bm->end()) {
		GetNameWriter()->Write('s', name);
		GetValueWriter()->Write(name, x);
	} else {
		GetNameWriter()->Write('v', name);
		GetFunctionWriter()->WriteSet(name, it->second);
	}
}

typedef map<int, string> PhysicalQuantityMap;

class PhysicalQuantityHandler : boost::noncopyable {
public:
	PhysicalQuantityHandler(boost::uuids::uuid uuid, PhysicalQuantityMap *pqm) : uuid_(uuid), pqm_(pqm), max_pq_id_() {}

	bool Handle(boost::uuids::uuid u, char /*type*/, int pq_id, const char *name, const char * /*unit*/, double /*capacity*/) {
		if (uuid_ != u) return true; // skip this entry
		pqm_->insert(make_pair(pq_id, string(name)));
		max_pq_id_ = std::max(pq_id, max_pq_id_);
		return true;
	}

	int max_pq_id() const {return max_pq_id_;}

private:
	boost::uuids::uuid uuid_;
	PhysicalQuantityMap *pqm_;
	int max_pq_id_;
};

class BridgeHandler : boost::noncopyable {
public:
	BridgeHandler(boost::uuids::uuid uuid, PhysicalQuantityMap *pqm)
		: uuid_(uuid),
		  pqm_(pqm)
	{}

	bool Handle(boost::uuids::uuid uuid, int pq_id, const char *direction, const char * /*sub_type*/, const char *connector)
	{
		if (uuid_ != uuid) return true;
		if (strcmp(direction, "get") == 0) {
			PhysicalQuantityMap::const_iterator it = pqm_->find(pq_id);
			if (it == pqm_->end()) {
				cerr << "failed to find physical-quantity: " << pq_id << endl;
				return false;
			}
			GetFunctionWriter()->WriteGet(it->second, connector);
		} else if (strcmp(direction, "set") == 0) {
			PhysicalQuantityMap::const_iterator it = pqm_->find(pq_id);
			if (it == pqm_->end()) {
				cerr << "failed to find physical-quantity: " << pq_id << endl;
				return false;
			}
			GetBridgeMap()->insert(make_pair("sbml:" + string(connector), it->second));
		} else {
			cerr << "invalid bridge: " << uuid << ":" << pq_id << endl;
			return false;
		}
		return true;
	}

private:
	boost::uuids::uuid uuid_;
	PhysicalQuantityMap *pqm_;
};

} // namespace

%}

/* Bison declarations */
%error-verbose /* obsolete directive standing for "%define parse.error verbose" in Bison 2.7 or later */

%union {
	int i;
	double d;
	char *name;
	Sexp *sexp;
}

%token NEWLINE

%token <i> INTEGER
%token <d> REAL
%token <name> NAME
%token <sexp> ID
%token <sexp> KEYWORD

%type <sexp> sexp seq0 seq1

%%
/* Grammer Rules */

input: /* empty */
    | input line
    ;

line: 'o' NAME REAL sexp NEWLINE {AddOde($2, $3, $4);free($2);}
    | 'o' NAME INTEGER sexp NEWLINE {AddOde($2, $3, $4);free($2);}
    | 'a' NAME sexp NEWLINE {AddAssignment($2, $3);free($2);}
    | 'c' NAME REAL NEWLINE {AddConstant($2, $3);free($2);}
    | 'c' NAME INTEGER NEWLINE {AddConstant($2, $3);free($2);}
    ;

sexp: REAL {$$ = new Sexp($1);}
    | INTEGER {$$ = new Sexp($1);}
    | ID
    | KEYWORD
    | '(' KEYWORD seq0 ')' {$$ = new Sexp($2, $3);}
    ;

seq0: /* empty */ {$$ = NULL;}
    | seq1
    ;

seq1: sexp seq0 {$$ = new Sexp($1, $2);};

%%
/* Epilogue */

void yyerror(char const *s)
{
	std::fprintf(stderr, "%s\n", s);
}

int main(int argc, char *argv[])
{
	po::options_description opts("options");
	po::positional_options_description popts;
	po::variables_map vm;
	string uuid, db_file, name_file, value_file, function_file, ode_file;
	int print_help = 0;

	opts.add_options()
		("uuid", po::value<string>(&uuid), "Input UUID")
		("db", po::value<string>(&db_file), "Input database file")
		("name", po::value<string>(&name_file), "Output name file")
		("value", po::value<string>(&value_file), "Output value file")
		("function", po::value<string>(&function_file), "Output function file")
		("ode", po::value<string>(&ode_file), "Output ODE file")
		("help,h", "Show this message");
	popts.add("uuid", 1).add("db", 1).add("name", 1).add("value", 1).add("function", 1).add("ode", 1);

	try {
		po::store(po::command_line_parser(argc, argv).options(opts).positional(popts).run(), vm);
		po::notify(vm);
		if (vm.count("help") > 0) {
			print_help = 1;
		} else if ( vm.count("uuid") == 0 ||
					vm.count("db") == 0 ||
					vm.count("name") == 0 ||
					vm.count("value") == 0 ||
					vm.count("function") == 0 ||
					vm.count("ode") == 0 ) {
			print_help = 2;
		}
	} catch (const po::error &) {
		print_help = 2;
	}
	if (print_help) {
		cerr << "usage: " << argv[0] << " UUID DB NAME VALUE FUNCTION ODE" << endl;
		cerr << opts;
		return (print_help == 1) ? EXIT_SUCCESS : EXIT_FAILURE;
	}

	boost::uuids::string_generator gen;
	boost::uuids::uuid u = gen(uuid.c_str());

	boost::scoped_ptr<db::Driver> driver(new db::Driver(db_file.c_str()));

	boost::scoped_ptr<PhysicalQuantityMap> pqm(new PhysicalQuantityMap);
	int max_pq_id = 0;
	{
		boost::scoped_ptr<db::NameLoader> loader(new db::NameLoader(driver->db()));
		boost::scoped_ptr<PhysicalQuantityHandler> handler(new PhysicalQuantityHandler(u, pqm.get()));
		if (!loader->Load(handler.get())) {
			return EXIT_FAILURE;
		}
		max_pq_id = handler->max_pq_id();
	}

	GetNameWriter()->set_pq_id(max_pq_id);
	GetNameWriter()->set_uuid(uuid.c_str());
	GetNameWriter()->OpenFile(name_file);
	GetValueWriter()->set_uuid(uuid.c_str());
	GetValueWriter()->OpenFile(value_file);
	GetFunctionWriter()->set_uuid(uuid.c_str());
	GetFunctionWriter()->OpenFile(function_file);
	GetOdeWriter()->set_uuid(uuid.c_str());
	GetOdeWriter()->OpenFile(ode_file);

	boost::scoped_ptr<BridgeMap> sm(new BridgeMap);
	{
		boost::scoped_ptr<db::BridgeLoader> loader(new db::BridgeLoader(driver->db()));
		boost::scoped_ptr<BridgeHandler> handler(new BridgeHandler(u, pqm.get()));
		if (!loader->Load(handler.get())) {
			return EXIT_FAILURE;
		}
	}

	return yyparse();
}
