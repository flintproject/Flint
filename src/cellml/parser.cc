/* -*- Mode: C++; tab-width: 4; indent-tabs-mode: t; c-basic-offset: 4 -*- vim:set ts=4 sw=4 sts=4 noet: */
#ifdef HAVE_CONFIG_H
#include "config.h"
#endif

#include "parser.h"

#include <cassert>
#include <cstdio>
#include <cstdlib>
#include <cstring>
#include <iostream>
#include <memory>
#include <sstream>

#include <libxml/xmlreader.h>

#include "db/query.h"
#include "mathml/math_dumper.h"
#include "sqlite3.h"
#include "modelpath.h"

using std::cerr;
using std::endl;
using std::strcmp;

namespace flint {
namespace {

bool IsInCellMLNamespace(const xmlChar *uri)
{
	static const char CELLML_NAMESPACE_HEAD[] = "http://www.cellml.org/cellml/";
	static const size_t kLength = sizeof(CELLML_NAMESPACE_HEAD)/sizeof(CELLML_NAMESPACE_HEAD[0])-1;
	if (!uri) return true;
	if (std::strncmp(CELLML_NAMESPACE_HEAD, (const char *)uri, kLength) == 0) return true;
	return false;
}

bool IsInMathMLNamespace(const xmlChar *uri)
{
	static const char MATHML_NAMESPACE_URI[] = "http://www.w3.org/1998/Math/MathML";
	if (!uri) return false;
	if (strcmp(MATHML_NAMESPACE_URI, (const char *)uri) == 0) return true;
	return false;
}

class CellMLUnits {
public:
	explicit CellMLUnits(xmlChar *name) : name_(name) {
		assert(name);
	}

	~CellMLUnits() {xmlFree(name_);}

	const xmlChar *name() const {return name_;}

private:
	xmlChar *name_;
};

class CellMLUnit {
public:
	CellMLUnit()
		: units_(),
		  prefix_(),
		  exponent_(),
		  multiplier_()
	{
	}

	~CellMLUnit() {
		if (units_) xmlFree(units_);
		if (prefix_) xmlFree(prefix_);
		if (exponent_) xmlFree(exponent_);
		if (multiplier_) xmlFree(multiplier_);
	}

	void set_units(xmlChar *units) {units_ = units;}
	void set_prefix(xmlChar *prefix) {prefix_ = prefix;}
	void set_exponent(xmlChar *exponent) {exponent_ = exponent;}
	void set_multiplier(xmlChar *multiplier) {multiplier_ = multiplier;}

	const xmlChar *units() const {return units_;}
	const xmlChar *prefix() const {return prefix_;}
	const xmlChar *exponent() const {return exponent_;}
	const xmlChar *multiplier() const {return multiplier_;}

private:
	xmlChar *units_;
	xmlChar *prefix_;
	xmlChar *exponent_;
	xmlChar *multiplier_;
};

class CellMLComponent {
public:
	explicit CellMLComponent(xmlChar *name) : name_(name) {
		assert(name);
	}

	~CellMLComponent() {xmlFree(name_);}

	const xmlChar *name() const {return name_;}

private:
	xmlChar *name_;
};

class CellMLVariable {
public:
	CellMLVariable()
		: name_(),
		  units_(),
		  public_interface_(),
		  private_interface_(),
		  initial_value_()
	{}

	~CellMLVariable() {
		if (name_) xmlFree(name_);
		if (units_) xmlFree(units_);
		if (public_interface_) xmlFree(public_interface_);
		if (private_interface_) xmlFree(private_interface_);
		if (initial_value_) xmlFree(initial_value_);
	}

	void set_name(xmlChar *name) {name_ = name;}
	void set_units(xmlChar *units) {units_ = units;}
	void set_public_interface(xmlChar *public_interface) {public_interface_ = public_interface;}
	void set_private_interface(xmlChar *private_interface) {private_interface_ = private_interface;}
	void set_initial_value(xmlChar *initial_value) {initial_value_ = initial_value;}

	const xmlChar *name() const {return name_;}
	const xmlChar *units() const {return units_;}
	const xmlChar *public_interface() const {return public_interface_;}
	const xmlChar *private_interface() const {return private_interface_;}
	const xmlChar *initial_value() const {return initial_value_;}

private:
	xmlChar *name_;
	xmlChar *units_;
	xmlChar *public_interface_;
	xmlChar *private_interface_;
	xmlChar *initial_value_;
};

class CellMLConnection {
public:
	CellMLConnection()
		: component_1_(),
		  component_2_(),
		  id_()
	{
	}

	~CellMLConnection() {
		if (component_1_) xmlFree(component_1_);
		if (component_2_) xmlFree(component_2_);
	}

	void set_component_1(xmlChar *component_1) {component_1_ = component_1;}
	void set_component_2(xmlChar *component_2) {component_2_ = component_2;}
	void set_id(sqlite3_int64 id) {id_ = id;}

	const xmlChar *component_1() const {return component_1_;}
	const xmlChar *component_2() const {return component_2_;}
	sqlite3_int64 id() const {return id_;}

private:
	xmlChar *component_1_;
	xmlChar *component_2_;
	sqlite3_int64 id_;
};

class CellMLMapVariables {
public:
	CellMLMapVariables()
		: variable_1_(),
		  variable_2_() {
	}

	~CellMLMapVariables() {
		if (variable_1_) xmlFree(variable_1_);
		if (variable_2_) xmlFree(variable_2_);
	}

	void set_variable_1(xmlChar *variable_1) {variable_1_ = variable_1;}
	void set_variable_2(xmlChar *variable_2) {variable_2_ = variable_2;}

	const xmlChar *variable_1() const {return variable_1_;}
	const xmlChar *variable_2() const {return variable_2_;}

private:
	xmlChar *variable_1_;
	xmlChar *variable_2_;
};

template<typename TParser>
class FormulaHandler {
public:
	FormulaHandler(TParser *parser, const CellMLComponent *component)
		: parser_(parser),
		  component_(component),
		  oss_()
	{
	}

	~FormulaHandler() {
	}

	std::ostringstream *oss() {return &oss_;}

	int Handle(int i) {
		sqlite3_stmt *stmt = parser_->stmt_maths();
		int e;
		e = sqlite3_bind_text(stmt, 1, (const char *)component_->name(),
							  -1, SQLITE_STATIC);
		if (e != SQLITE_OK) {
			cerr << "failed to bind parameter: " << e << endl;
			return -2;
		}
		std::string body = oss_.str();
		e = sqlite3_bind_text(stmt, 2, body.c_str(),
							  -1, SQLITE_STATIC);
		if (e != SQLITE_OK) {
			cerr << "failed to bind parameter: " << e << endl;
			return -2;
		}
		e = sqlite3_step(stmt);
		if (e != SQLITE_DONE) {
			cerr << "failed to step statement: " << e << endl;
			return -2;
		}
		sqlite3_reset(stmt);
		// reset buffer
		oss_.clear();
		oss_.str("");
		return i;
	}

private:
	TParser *parser_;
	const CellMLComponent *component_;
	std::ostringstream oss_;
};

template<typename TParser>
class CellMLReader {
public:
	CellMLReader(const char *path, TParser *parser)
		: parser_(parser)
	{
		assert(path);
		assert(parser);
		xmlInitParser();
		text_reader_ = xmlReaderForFile(path, NULL, 0);
	}

	~CellMLReader() {
		xmlFreeTextReader(text_reader_);
		xmlCleanupParser();
	}

	int Read() {
		int i = xmlTextReaderRead(text_reader_);
		while (i > 0) {
			const xmlChar *uri = xmlTextReaderConstNamespaceUri(text_reader_);
			if (!IsInCellMLNamespace(uri)) {
				i = xmlTextReaderRead(text_reader_);
				continue;
			}
			int type = xmlTextReaderNodeType(text_reader_);
			if (type == XML_READER_TYPE_ELEMENT) {
				const xmlChar *local_name = xmlTextReaderConstLocalName(text_reader_);
				if (xmlStrEqual(local_name, BAD_CAST "units")) {
					i = ReadUnits(NULL);
					if (i <= 0) return i;
					continue;
				} else if (xmlStrEqual(local_name, BAD_CAST "component")) {
					i = ReadComponent();
					if (i <= 0) return i;
					continue;
				} else if (xmlStrEqual(local_name, BAD_CAST "connection")) {
					i = ReadConnection();
					if (i <= 0) return i;
					continue;
				} else {
					// ignored
				}
			}
			i = xmlTextReaderRead(text_reader_);
		}
		return i;
	}

private:
	int ReadUnits(const CellMLComponent *component) {
		std::unique_ptr<CellMLUnits> units;
		int i;
		while ( (i = xmlTextReaderMoveToNextAttribute(text_reader_)) > 0) {
			if (xmlTextReaderIsNamespaceDecl(text_reader_)) continue;
			const xmlChar *uri = xmlTextReaderConstNamespaceUri(text_reader_);
			if (!IsInCellMLNamespace(uri)) continue;

			const xmlChar *local_name = xmlTextReaderConstLocalName(text_reader_);
			if (xmlStrEqual(local_name, BAD_CAST "name")) {
				units.reset(new CellMLUnits(xmlTextReaderValue(text_reader_)));
				break;
			}
		}
		if (!units) {
			cerr << "no name attribute of <units>" << endl;
			return -2;
		}
		i = xmlTextReaderRead(text_reader_);
		while (i > 0) {
			const xmlChar *uri = xmlTextReaderConstNamespaceUri(text_reader_);
			if (!IsInCellMLNamespace(uri)) {
				i = xmlTextReaderRead(text_reader_);
				continue;
			}
			int type = xmlTextReaderNodeType(text_reader_);
			if (type == XML_READER_TYPE_ELEMENT) {
				const xmlChar *local_name = xmlTextReaderConstLocalName(text_reader_);
				if (xmlStrEqual(local_name, BAD_CAST "unit")) {
					i = ReadUnit(component, units.get());
					if (i <= 0) return i;
					continue;
				}
			} else if (type == XML_READER_TYPE_END_ELEMENT) {
				const xmlChar *local_name = xmlTextReaderConstLocalName(text_reader_);
				if (xmlStrEqual(local_name, BAD_CAST "units")) {
					return xmlTextReaderRead(text_reader_);
				}
			}
			i = xmlTextReaderRead(text_reader_);
		}
		return i;
	}

	int ReadUnit(const CellMLComponent *component, const CellMLUnits *units) {
		std::unique_ptr<CellMLUnit> unit(new CellMLUnit);
		int i;
		while ( (i = xmlTextReaderMoveToNextAttribute(text_reader_)) > 0) {
			if (xmlTextReaderIsNamespaceDecl(text_reader_)) continue;

			const xmlChar *local_name = xmlTextReaderConstLocalName(text_reader_);
			if (xmlStrEqual(local_name, BAD_CAST "units")) {
				unit->set_units(xmlTextReaderValue(text_reader_));
			} else if (xmlStrEqual(local_name, BAD_CAST "prefix")) {
				unit->set_prefix(xmlTextReaderValue(text_reader_));
			} else if (xmlStrEqual(local_name, BAD_CAST "exponent")) {
				unit->set_exponent(xmlTextReaderValue(text_reader_));
			} else if (xmlStrEqual(local_name, BAD_CAST "multiplier")) {
				unit->set_multiplier(xmlTextReaderValue(text_reader_));
			} else {
				cerr << "unknown attribute of <unit>: " << local_name << endl;
				return -2;
			}
		}
		if (!unit->units()) {
			cerr << "no units attribute of <unit>: "
				 << units->name()
				 << endl;
			return -2;
		}
		// insert a row into units
		sqlite3_stmt *stmt = parser_->stmt_units();
		int e;
		e = sqlite3_bind_text(stmt, 1, (const char *)units->name(),
							  -1, SQLITE_STATIC);
		if (e != SQLITE_OK) {
			cerr << "failed to bind parameter: " << e << endl;
			return -2;
		}
		e = sqlite3_bind_text(stmt, 2, (const char *)unit->units(),
							  -1, SQLITE_STATIC);
		if (e != SQLITE_OK) {
			cerr << "failed to bind parameter: " << e << endl;
			return -2;
		}
		e = sqlite3_bind_text(stmt, 3, (const char *)unit->prefix(),
							  -1, SQLITE_STATIC);
		if (e != SQLITE_OK) {
			cerr << "failed to bind parameter: " << e << endl;
			return -2;
		}
		e = sqlite3_bind_text(stmt, 4, (const char *)unit->exponent(),
							  -1, SQLITE_STATIC);
		if (e != SQLITE_OK) {
			cerr << "failed to bind parameter: " << e << endl;
			return -2;
		}
		e = sqlite3_bind_text(stmt, 5, (const char *)unit->multiplier(),
							  -1, SQLITE_STATIC);
		if (e != SQLITE_OK) {
			cerr << "failed to bind parameter: " << e << endl;
			return -2;
		}
		e = sqlite3_bind_text(stmt, 6, (component) ? (const char *)component->name() : NULL,
							  -1, SQLITE_STATIC);
		if (e != SQLITE_OK) {
			cerr << "failed to bind parameter: " << e << endl;
			return -2;
		}
		e = sqlite3_step(stmt);
		if (e != SQLITE_DONE) {
			cerr << "failed to step statement: " << e << endl;
			return -2;
		}
		sqlite3_reset(stmt);
		return xmlTextReaderRead(text_reader_);
	}

	int ReadComponent() {
		std::unique_ptr<CellMLComponent> component;
		int i;
		while ( (i = xmlTextReaderMoveToNextAttribute(text_reader_)) > 0) {
			if (xmlTextReaderIsNamespaceDecl(text_reader_)) continue;
			const xmlChar *uri = xmlTextReaderConstNamespaceUri(text_reader_);
			if (!IsInCellMLNamespace(uri)) continue;

			const xmlChar *local_name = xmlTextReaderConstLocalName(text_reader_);
			if (xmlStrEqual(local_name, BAD_CAST "name")) {
				xmlChar *name = xmlTextReaderValue(text_reader_);
				component.reset(new CellMLComponent(name));
				break;
			}
		}
		if (!component) {
			cerr << "no name attribute of <component>" << endl;
			return -2;
		}
		i = xmlTextReaderRead(text_reader_);
		while (i > 0) {
			int type = xmlTextReaderNodeType(text_reader_);
			const xmlChar *uri = xmlTextReaderConstNamespaceUri(text_reader_);
			if (IsInMathMLNamespace(uri)) {
				if (type == XML_READER_TYPE_ELEMENT) {
					const xmlChar *local_name = xmlTextReaderConstLocalName(text_reader_);
					if (xmlStrEqual(local_name, BAD_CAST "math")) {
						std::unique_ptr<FormulaHandler<TParser> > handler(new FormulaHandler<TParser>(parser_, component.get()));
						std::unique_ptr<mathml::MathDumper> math_dumper(new mathml::MathDumper(text_reader_, handler->oss()));
						i = math_dumper->Read(handler.get());
						if (i <= 0) return i;
						continue;
					}
				}
				i = xmlTextReaderRead(text_reader_);
				continue;
			}
			if (!IsInCellMLNamespace(uri)) {
				i = xmlTextReaderRead(text_reader_);
				continue;
			}
			if (type == XML_READER_TYPE_ELEMENT) {
				const xmlChar *local_name = xmlTextReaderConstLocalName(text_reader_);
				if (xmlStrEqual(local_name, BAD_CAST "units")) {
					i = ReadUnits(component.get());
					if (i <= 0) return i;
					continue;
				} else if (xmlStrEqual(local_name, BAD_CAST "variable")) {
					i = ReadVariable(component.get());
					if (i <= 0) return i;
					continue;
				} else if (xmlStrEqual(local_name, BAD_CAST "role")) {
					// TODO
					cerr << "<role> is not yet supported" << endl;
				} else {
					// TODO
					cerr << "skip <"
						 << local_name
						 << ">"
						 << endl;
				}
			} else if (type == XML_READER_TYPE_END_ELEMENT) {
				const xmlChar *local_name = xmlTextReaderConstLocalName(text_reader_);
				if (xmlStrEqual(local_name, BAD_CAST "component")) {
					return xmlTextReaderRead(text_reader_);
				}
			}
			i = xmlTextReaderRead(text_reader_);
		}
		return i;
	}

	int ReadVariable(const CellMLComponent *component) {
		std::unique_ptr<CellMLVariable> variable(new CellMLVariable);
		int i;
		while ( (i = xmlTextReaderMoveToNextAttribute(text_reader_)) > 0) {
			if (xmlTextReaderIsNamespaceDecl(text_reader_)) continue;
			const xmlChar *uri = xmlTextReaderConstNamespaceUri(text_reader_);
			if (!IsInCellMLNamespace(uri)) continue;

			const xmlChar *local_name = xmlTextReaderConstLocalName(text_reader_);
			if (xmlStrEqual(local_name, BAD_CAST "name")) {
				variable->set_name(xmlTextReaderValue(text_reader_));
			} else if (xmlStrEqual(local_name, BAD_CAST "units")) {
				variable->set_units(xmlTextReaderValue(text_reader_));
			} else if (xmlStrEqual(local_name, BAD_CAST "initial_value")) {
				variable->set_initial_value(xmlTextReaderValue(text_reader_));
			} else if (xmlStrEqual(local_name, BAD_CAST "public_interface")) {
				variable->set_public_interface(xmlTextReaderValue(text_reader_));
			} else if (xmlStrEqual(local_name, BAD_CAST "private_interface")) {
				variable->set_private_interface(xmlTextReaderValue(text_reader_));
			} else {
				cerr << "unknown attribute of <variable>: "
					 << local_name
					 << " in "
					 << component->name()
					 << endl;
				return -2;
			}
		}
		// insert a row into variables
		sqlite3_stmt *stmt = parser_->stmt_variables();
		int e;
		e = sqlite3_bind_text(stmt, 1, (const char *)component->name(),
							  -1, SQLITE_STATIC);
		if (e != SQLITE_OK) {
			cerr << "failed to bind parameter: " << e << endl;
			return -2;
		}
		e = sqlite3_bind_text(stmt, 2, (const char *)variable->name(),
							  -1, SQLITE_STATIC);
		if (e != SQLITE_OK) {
			cerr << "failed to bind parameter: " << e << endl;
			return -2;
		}
		e = sqlite3_bind_text(stmt, 3, (const char *)variable->units(),
							  -1, SQLITE_STATIC);
		if (e != SQLITE_OK) {
			cerr << "failed to bind parameter: " << e << endl;
			return -2;
		}
		e = sqlite3_bind_text(stmt, 4, (const char *)variable->public_interface(),
							  -1, SQLITE_STATIC);
		if (e != SQLITE_OK) {
			cerr << "failed to bind parameter: " << e << endl;
			return -2;
		}
		e = sqlite3_bind_text(stmt, 5, (const char *)variable->private_interface(),
							  -1, SQLITE_STATIC);
		if (e != SQLITE_OK) {
			cerr << "failed to bind parameter: " << e << endl;
			return -2;
		}
		e = sqlite3_bind_text(stmt, 6, (const char *)variable->initial_value(),
							  -1, SQLITE_STATIC);
		if (e != SQLITE_OK) {
			cerr << "failed to bind parameter: " << e << endl;
			return -2;
		}
		e = sqlite3_step(stmt);
		if (e != SQLITE_DONE) {
			cerr << "failed to step statement: " << e << endl;
			return -2;
		}
		sqlite3_reset(stmt);
		return xmlTextReaderRead(text_reader_);
	}

	int ReadConnection() {
		std::unique_ptr<CellMLConnection> connection;
		int i = xmlTextReaderRead(text_reader_);
		while (i > 0) {
			const xmlChar *uri = xmlTextReaderConstNamespaceUri(text_reader_);
			if (!IsInCellMLNamespace(uri)) {
				i = xmlTextReaderRead(text_reader_);
				continue;
			}
			int type = xmlTextReaderNodeType(text_reader_);
			if (type == XML_READER_TYPE_ELEMENT) {
				const xmlChar *local_name = xmlTextReaderConstLocalName(text_reader_);
				if (xmlStrEqual(local_name, BAD_CAST "map_components")) {
					connection.reset(new CellMLConnection);
					i = ReadMapComponents(connection.get());
					if (i <= 0) return i;
					continue;
				} else if (xmlStrEqual(local_name, BAD_CAST "map_variables")) {
					if (!connection) {
						cerr << "no leading <map_components> found" << endl;
						return -2;
					}
					i = ReadMapVariables(connection.get());
					if (i <= 0) return i;
					continue;
				} else {
					cerr << "unknown child of <connection>: "
						 << local_name
						 << endl;
					return -2;
				}
			} else if (type == XML_READER_TYPE_END_ELEMENT) {
				const xmlChar *local_name = xmlTextReaderConstLocalName(text_reader_);
				if (xmlStrEqual(local_name, BAD_CAST "connection")) {
					return xmlTextReaderRead(text_reader_);
				}
			}
			i = xmlTextReaderRead(text_reader_);
		}
		return i;
	}

	int ReadMapComponents(CellMLConnection *connection) {
		int i;
		while ( (i = xmlTextReaderMoveToNextAttribute(text_reader_)) > 0) {
			if (xmlTextReaderIsNamespaceDecl(text_reader_)) continue;
			const xmlChar *uri = xmlTextReaderConstNamespaceUri(text_reader_);
			if (!IsInCellMLNamespace(uri)) continue;

			const xmlChar *local_name = xmlTextReaderConstLocalName(text_reader_);
			if (xmlStrEqual(local_name, BAD_CAST "component_1")) {
				connection->set_component_1(xmlTextReaderValue(text_reader_));
			} else if (xmlStrEqual(local_name, BAD_CAST "component_2")) {
				connection->set_component_2(xmlTextReaderValue(text_reader_));
			} else {
				cerr << "unknown attribute of <map_components>: "
					 << local_name
					 << endl;
				return -2;
			}
		}
		if (!connection->component_1()) {
			cerr << "no component_1 attribute of <map_components>" << endl;
			return -2;
		}
		if (!connection->component_2()) {
			cerr << "no component_2 attribute of <map_components>" << endl;
			return -2;
		}
		// insert a row into connections
		sqlite3_stmt *stmt = parser_->stmt_connections();
		int e;
		e = sqlite3_bind_text(stmt, 1, (const char *)connection->component_1(),
							  -1, SQLITE_STATIC);
		if (e != SQLITE_OK) {
			cerr << "failed to bind parameter: " << e << endl;
			return -2;
		}
		e = sqlite3_bind_text(stmt, 2, (const char *)connection->component_2(),
							  -1, SQLITE_STATIC);
		if (e != SQLITE_OK) {
			cerr << "failed to bind parameter: " << e << endl;
			return -2;
		}
		e = sqlite3_step(stmt);
		if (e != SQLITE_DONE) {
			cerr << "failed to step statement: " << e << endl;
			return -2;
		}
		sqlite3_reset(stmt);
		int id = sqlite3_last_insert_rowid(parser_->db());
		connection->set_id(id);
		return xmlTextReaderRead(text_reader_);
	}

	int ReadMapVariables(CellMLConnection *connection) {
		std::unique_ptr<CellMLMapVariables> map_variables(new CellMLMapVariables);
		int i;
		while ( (i = xmlTextReaderMoveToNextAttribute(text_reader_)) > 0) {
			if (xmlTextReaderIsNamespaceDecl(text_reader_)) continue;
			const xmlChar *uri = xmlTextReaderConstNamespaceUri(text_reader_);
			if (!IsInCellMLNamespace(uri)) continue;

			const xmlChar *local_name = xmlTextReaderConstLocalName(text_reader_);
			if (xmlStrEqual(local_name, BAD_CAST "variable_1")) {
				map_variables->set_variable_1(xmlTextReaderValue(text_reader_));
			} else if (xmlStrEqual(local_name, BAD_CAST "variable_2")) {
				map_variables->set_variable_2(xmlTextReaderValue(text_reader_));
			} else {
				cerr << "unknown attribute of <map_components>: "
					 << local_name
					 << endl;
				return -2;
			}
		}
		if (!map_variables->variable_1()) {
			cerr << "no variable_1 attribute of <map_variables>" << endl;
			return -2;
		}
		if (!map_variables->variable_2()) {
			cerr << "no variable_2 attribute of <map_variables>" << endl;
			return -2;
		}
		// insert a row into map_variables
		sqlite3_stmt *stmt = parser_->stmt_map_variables();
		int e;
		e = sqlite3_bind_int64(stmt, 1, connection->id());
		if (e != SQLITE_OK) {
			cerr << "failed to bind parameter: " << e << endl;
			return -2;
		}
		e = sqlite3_bind_text(stmt, 2, (const char *)map_variables->variable_1(),
							  -1, SQLITE_STATIC);
		if (e != SQLITE_OK) {
			cerr << "failed to bind parameter: " << e << endl;
			return -2;
		}
		e = sqlite3_bind_text(stmt, 3, (const char *)map_variables->variable_2(),
							  -1, SQLITE_STATIC);
		if (e != SQLITE_OK) {
			cerr << "failed to bind parameter: " << e << endl;
			return -2;
		}
		e = sqlite3_step(stmt);
		if (e != SQLITE_DONE) {
			cerr << "failed to step statement: " << e << endl;
			return -2;
		}
		sqlite3_reset(stmt);
		return xmlTextReaderRead(text_reader_);
	}

	TParser *parser_;
	xmlTextReaderPtr text_reader_;
};

class CellMLParser {
public:
	explicit CellMLParser(sqlite3 *db)
		: db_(db),
		  stmt_units_(),
		  stmt_variables_(),
		  stmt_maths_(),
		  stmt_connections_(),
		  stmt_map_variables_()
	{}

	~CellMLParser() {
		sqlite3_finalize(stmt_units_);
		sqlite3_finalize(stmt_variables_);
		sqlite3_finalize(stmt_maths_);
		sqlite3_finalize(stmt_connections_);
		sqlite3_finalize(stmt_map_variables_);
	}

	sqlite3 *db() const {return db_;}
	sqlite3_stmt *stmt_units() const {return stmt_units_;}
	sqlite3_stmt *stmt_variables() const {return stmt_variables_;}
	sqlite3_stmt *stmt_maths() const {return stmt_maths_;}
	sqlite3_stmt *stmt_connections() const {return stmt_connections_;}
	sqlite3_stmt *stmt_map_variables() const {return stmt_map_variables_;}

	bool Parse() {
		std::unique_ptr<char[]> model_file(GetModelFilename(db_));
		if (!BeginTransaction(db_)) return false;
		if (!CreateTables()) return false;
		if (!PrepareStatements()) return false;
		CellMLReader<CellMLParser> reader(model_file.get(), this);
		return reader.Read() == 0 && CommitTransaction(db_);
	}

private:
	bool CreateTables() {
		if (!CreateTable(db_, "units", "(name TEXT, units TEXT, prefix TEXT, exponent TEXT, multiplier TEXT, component TEXT)"))
			return false;
		if (!CreateTable(db_, "variables", "(component TEXT, name TEXT, units TEXT, public_interface TEXT, private_interface TEXT, initial_value TEXT)"))
			return false;
		if (!CreateTable(db_, "maths", "(component TEXT, body TEXT)"))
			return false;
		if (!CreateTable(db_, "connections", "(component_1 TEXT, component_2 TEXT)"))
			return false;
		if (!CreateTable(db_, "map_variables", "(connection_id INTEGER, variable_1 TEXT, variable_2 TEXT)"))
			return false;
		// subsequent tables
		if (!CreateTable(db_, "time_unit", "(name TEXT)"))
			return false;
		return true;
	}

	bool PrepareStatements() {
		int e;
		sqlite3_stmt *stmt;

		e = sqlite3_prepare_v2(db_,
							   "INSERT INTO units VALUES (?, ?, ?, ?, ?, ?)",
							   -1, &stmt, NULL);
		if (e != SQLITE_OK) {
			cerr << "failed to prepare statement: " << e << endl;
			return false;
		}
		stmt_units_ = stmt;

		e = sqlite3_prepare_v2(db_,
							   "INSERT INTO variables VALUES (?, ?, ?, ?, ?, ?)",
							   -1, &stmt, NULL);
		if (e != SQLITE_OK) {
			cerr << "failed to prepare statement: " << e << endl;
			return false;
		}
		stmt_variables_ = stmt;

		e = sqlite3_prepare_v2(db_,
							   "INSERT INTO maths VALUES (?, ?)",
							   -1, &stmt, NULL);
		if (e != SQLITE_OK) {
			cerr << "failed to prepare statement: " << e << endl;
			return false;
		}
		stmt_maths_ = stmt;

		e = sqlite3_prepare_v2(db_,
							   "INSERT INTO connections VALUES (?, ?)",
							   -1, &stmt, NULL);
		if (e != SQLITE_OK) {
			cerr << "failed to prepare statement: " << e << endl;
			return false;
		}
		stmt_connections_ = stmt;

		e = sqlite3_prepare_v2(db_,
							   "INSERT INTO map_variables VALUES (?, ?, ?)",
							   -1, &stmt, NULL);
		if (e != SQLITE_OK) {
			cerr << "failed to prepare statement: " << e << endl;
			return false;
		}
		stmt_map_variables_ = stmt;

		return true;
	}

	sqlite3 *db_;
	sqlite3_stmt *stmt_units_;
	sqlite3_stmt *stmt_variables_;
	sqlite3_stmt *stmt_maths_;
	sqlite3_stmt *stmt_connections_;
	sqlite3_stmt *stmt_map_variables_;
};

} // namespace

bool ParseCellml(sqlite3 *db)
{
	LIBXML_TEST_VERSION

	CellMLParser parser(db);
	return parser.Parse();
}

}
