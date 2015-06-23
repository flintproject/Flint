/* -*- Mode: C++; tab-width: 4; indent-tabs-mode: t; c-basic-offset: 4 -*- vim:set ts=4 sw=4 sts=4 noet: */
#ifdef HAVE_CONFIG_H
#include "config.h"
#endif

#include "phsp.hh"

#include <cstdio>
#include <cstdlib>
#include <cstring>
#include <iostream>
#include <sstream>
#include <string>
#include <map>

#define BOOST_FILESYSTEM_NO_DEPRECATED
#include <boost/filesystem.hpp>
#include <boost/noncopyable.hpp>
#include <boost/scoped_array.hpp>
#include <boost/scoped_ptr.hpp>

#include <libxml/xmlreader.h>

#include "base/rational.h"
#include "db/query.h"
#include "mathml/math_dumper.h"
#include "sqlite3.h"
#include "utf8path.h"

using std::cerr;
using std::endl;
using std::fclose;
using std::fopen;
using std::fprintf;
using std::perror;
using std::sprintf;
using std::string;

namespace phsp {

namespace {

class Model : boost::noncopyable {
public:
	Model() : format_(NULL), iref_(NULL) {}

	~Model() {
		if (format_) xmlFree(format_);
		if (iref_) xmlFree(iref_);
	}

	const xmlChar *format() const {return format_;}
	const xmlChar *iref() const {return iref_;}

	void set_format(xmlChar *format) {format_ = format;}
	void set_iref(xmlChar *iref) {iref_ = iref;}

private:
	xmlChar *format_;
	xmlChar *iref_;
};

class Parameter : boost::noncopyable {
public:
	explicit Parameter(xmlChar *name) : name_(name) {}

	~Parameter() {
		if (name_) xmlFree(name_);
	}

	const xmlChar *name() const {return name_;}

private:
	xmlChar *name_;
};

class Range : boost::noncopyable {
public:
	enum Type {
		kUnspecified,
		kInterval,
		kEnum
	};

	Range() : type_(kUnspecified) {}

	Type type() const {return type_;}
	const boost::rational<long> &lower() const {return lower_;}
	const boost::rational<long> &upper() const {return upper_;}
	const boost::rational<long> &step() const {return step_;}

	void set_type(Type type) {type_ = type;}
	void set_lower(const boost::rational<long> &lower) {lower_ = lower;}
	void set_upper(const boost::rational<long> &upper) {upper_ = upper;}
	void set_step(const boost::rational<long> &step) {step_ = step;}

	bool IsValid() {
		if (type_ == kUnspecified) {
			cerr << "type is unspecified" << endl;
			return false;
		}
		if (type_ == kInterval) {
			if (upper_ < lower_) {
				cerr << "the upper value ("
					 << upper_
					 << ") is smaller than the lower one ("
					 << lower_
					 << ")"
					 << endl;
				return false;
			}
			if (step_ <= 0) {
				cerr << "the step value is non-positive: " << step_ << endl;
				return false;
			}
		}
		return true;
	}

private:
	Type type_;
	boost::rational<long> lower_;
	boost::rational<long> upper_;
	boost::rational<long> step_;
};

class Target : boost::noncopyable {
public:
	Target()
		: uuid_(NULL),
		  physical_quantity_id_(),
		  species_id_(NULL),
		  parameter_id_(NULL),
		  reaction_id_(NULL)
	{}

	~Target() {
		if (uuid_) xmlFree(uuid_);
		if (species_id_) xmlFree(species_id_);
		if (parameter_id_) xmlFree(parameter_id_);
		if (reaction_id_) xmlFree(reaction_id_);
	}

	const xmlChar *uuid() const {return uuid_;}
	int physical_quantity_id() const {return physical_quantity_id_;}
	const xmlChar *species_id() const {return species_id_;}
	const xmlChar *parameter_id() const {return parameter_id_;}
	const xmlChar *reaction_id() const {return reaction_id_;}

	void set_uuid(xmlChar *uuid) {uuid_ = uuid;}
	void set_physical_quantity_id(int id) {physical_quantity_id_ = id;}
	void set_species_id(xmlChar *id) {species_id_ = id;}
	void set_parameter_id(xmlChar *id) {parameter_id_ = id;}
	void set_reaction_id(xmlChar *id) {reaction_id_ = id;}

private:
	xmlChar *uuid_;
	int physical_quantity_id_;
	xmlChar *species_id_;
	xmlChar *parameter_id_;
	xmlChar *reaction_id_;
};

class Reader {
public:
	Reader(xmlTextReaderPtr &text_reader, sqlite3 *db)
		: text_reader_(text_reader)
		, db_(db)
	{
	}

	~Reader() {
		xmlFreeTextReader(text_reader_);
	}

	int Read(const boost::filesystem::path &cp) {
		int i = xmlTextReaderRead(text_reader_);
		while (i > 0) {
			int type = xmlTextReaderNodeType(text_reader_);
			if (type == XML_READER_TYPE_ELEMENT) {
				const xmlChar *local_name = xmlTextReaderConstLocalName(text_reader_);
				if (xmlStrEqual(local_name, BAD_CAST "phsp")) {
					// skip the top element
				} else if (xmlStrEqual(local_name, BAD_CAST "model")) {
					i = ReadModel(cp);
					if (i <= 0) return i;
					continue;
				} else {
					cerr << "unknown child of <phsp>: " << local_name << endl;
					return -2;
				}
			} else if (type == XML_READER_TYPE_END_ELEMENT) {
				const xmlChar *local_name = xmlTextReaderConstLocalName(text_reader_);
				if (xmlStrEqual(local_name, BAD_CAST "phsp")) {
					return 1;
				}
			}
			i = xmlTextReaderRead(text_reader_);
		}
		return i;
	}

private:
	int ReadModel(const boost::filesystem::path &cp) {
		boost::scoped_ptr<Model> model(new Model);
		int i;
		while ( (i = xmlTextReaderMoveToNextAttribute(text_reader_)) > 0) {
			if (xmlTextReaderIsNamespaceDecl(text_reader_)) continue;

			const xmlChar *local_name = xmlTextReaderConstLocalName(text_reader_);
			if (xmlStrEqual(local_name, BAD_CAST "format")) {
				model->set_format(xmlTextReaderValue(text_reader_));
				if ( !xmlStrEqual(model->format(), BAD_CAST "phml") &&
					 !xmlStrEqual(model->format(), BAD_CAST "sbml") ) {
					cerr << "unknown format of <model>: " << (const char *)model->format() << endl;
					return -2;
				}
			} else if (xmlStrEqual(local_name, BAD_CAST "iref")) {
				model->set_iref(xmlTextReaderValue(text_reader_));
			}
		}
		if (!model->format()) {
			cerr << "missing format of <model>" << endl;
			return -2;
		}
		if (!model->iref()) {
			cerr << "missing iref of <model>" << endl;
			return -2;
		}

		// search corresponding tasks to the model
		sqlite3_stmt *stmt;
		int e;
		e = sqlite3_prepare_v2(db_,
							   "SELECT tasks.rowid FROM tasks"
							   " LEFT JOIN models ON tasks.model_id = models.rowid"
							   " WHERE models.model_path = ?"
							   " ORDER BY tasks.rowid DESC",
							   -1, &stmt, NULL);
		if (e != SQLITE_OK) {
			cerr << "failed to prepare statement: "
				 << e << " (" << __FILE__ << ":" << __LINE__ << ")"
				 << endl;
			return -2;
		}
		e = sqlite3_bind_text(stmt, 1, (const char *)model->iref(), -1, SQLITE_STATIC);
		if (e != SQLITE_OK) {
			cerr << "failed to bind parameter: " << e << endl;
			return -2;
		}
		e = sqlite3_step(stmt);
		if (e == SQLITE_DONE) {
			// skip this <model>
			i = xmlTextReaderRead(text_reader_);
			while (i > 0) {
				int type = xmlTextReaderNodeType(text_reader_);
				if (type == XML_READER_TYPE_END_ELEMENT) {
					const xmlChar *local_name = xmlTextReaderConstLocalName(text_reader_);
					if (xmlStrEqual(local_name, BAD_CAST "model")) {
						return xmlTextReaderRead(text_reader_);
					}
				}
				i = xmlTextReaderRead(text_reader_);
			}
			return i;
		}
		if (e != SQLITE_ROW) {
			cerr << "failed to find a corresponding model: "
				 << (const char *)model->iref()
				 << endl;
			return -2;
		}
		int rowid = static_cast<int>(sqlite3_column_int64(stmt, 0));
		sqlite3_finalize(stmt);

		// create a task directory
		char dir_path[1024];
		sprintf(dir_path, "%d", rowid);
		boost::filesystem::path dp(dir_path);
		if (!boost::filesystem::is_directory(dp) && !boost::filesystem::create_directory(dp)) {
			cerr << "failed to create a directory: "
				 << dir_path
				 << endl;
			return -2;
		}

		boost::filesystem::path mp = GetPathFromUtf8((const char *)model->iref());
		boost::filesystem::path amp = boost::filesystem::absolute(mp, cp);
		boost::scoped_array<char> utf8amp(GetUtf8FromPath(amp));

		// update the model entry
		e = sqlite3_prepare_v2(db_, "UPDATE models SET absolute_path = ? WHERE rowid = ?",
							   -1, &stmt, NULL);
		if (e != SQLITE_OK) {
			cerr << "failed to prepare statement: "
				 << e << " (" << __FILE__ << ":" << __LINE__ << ")"
				 << endl;
			return -2;
		}
		e = sqlite3_bind_text(stmt, 1, utf8amp.get(), -1, SQLITE_STATIC);
		if (e != SQLITE_OK) {
			cerr << "failed to bind absolute_path: " << e << endl;
			return -2;
		}
		e = sqlite3_bind_int64(stmt, 2, rowid);
		if (e != SQLITE_OK) {
			cerr << "failed to bind rowid: " << e << endl;
			return -2;
		}
		e = sqlite3_step(stmt);
		if (e != SQLITE_DONE) {
			cerr << "failed to step statement: " << e << endl;
			return -2;
		}
		sqlite3_finalize(stmt);

		// attach a specific database for the model
		char query[1024];
		sprintf(query, "ATTACH DATABASE '%d/db' AS 'db%d'", rowid, rowid);
		char *em;
		e = sqlite3_exec(db_, query, NULL, NULL, &em);
		if (e != SQLITE_OK) {
			cerr << "failed to attach database: " << e
				 << ": " << em << endl;
			sqlite3_free(em);
			return -2;
		}

		// begin a transaction per attached session
		if (!BeginTransaction(db_))
			return -2;

		char table_name[64]; // long enough
		// save model's format
		sprintf(table_name, "db%d.model", rowid);
		if (!CreateTable(db_, table_name, "(format TEXT)"))
			return -2;
		sprintf(query, "INSERT INTO db%d.model VALUES (?)", rowid);
		e = sqlite3_prepare_v2(db_, query, -1, &stmt, NULL);
		if (e != SQLITE_OK) {
			cerr << "failed to prepare statement: "
				 << e << " (" << __FILE__ << ":" << __LINE__ << ")"
				 << endl;
			return -2;
		}
		e = sqlite3_bind_text(stmt, 1, (const char *)model->format(), -1, SQLITE_STATIC);
		if (e != SQLITE_OK) {
			cerr << "failed to bind parameter: " << e << endl;
			return -2;
		}
		e = sqlite3_step(stmt);
		if (e != SQLITE_DONE) {
			cerr << "failed to step statement: " << e << endl;
			return -2;
		}
		sqlite3_finalize(stmt);

		// prepare schemas
		sprintf(table_name, "db%d.phsp_parameters", rowid);
		if (!CreateTable(db_, table_name, "(name TEXT, range TEXT)"))
			return -2;
		sprintf(table_name, "db%d.phsp_targets", rowid);
		if (!CreateTable(db_, table_name, "(uuid TEXT, id TEXT, math TEXT)"))
			return -2;

		i = xmlTextReaderRead(text_reader_);
		while (i > 0) {
			int type = xmlTextReaderNodeType(text_reader_);
			if (type == XML_READER_TYPE_ELEMENT) {
				const xmlChar *local_name = xmlTextReaderConstLocalName(text_reader_);
				if (xmlStrEqual(local_name, BAD_CAST "parameter-set")) {
					i = ReadParameterSet(rowid);
					if (i <= 0) return i;
					continue;
				} else if (xmlStrEqual(local_name, BAD_CAST "target-set")) {
					i = ReadTargetSet(rowid);
					if (i <= 0) return i;
					continue;
				}
			} else if (type == XML_READER_TYPE_END_ELEMENT) {
				const xmlChar *local_name = xmlTextReaderConstLocalName(text_reader_);
				if (xmlStrEqual(local_name, BAD_CAST "model")) {
					// commit the transaction per attached session
					if (!CommitTransaction(db_))
						return -2;
					// detach the database after the transaction
					sprintf(query, "DETACH DATABASE 'db%d'", rowid);
					e = sqlite3_exec(db_, query, NULL, NULL, &em);
					if (e != SQLITE_OK) {
						cerr << "failed to detach database: " << e
							 << ": " << em << endl;
						sqlite3_free(em);
						return -2;
					}
					return xmlTextReaderRead(text_reader_);
				}
			}
			i = xmlTextReaderRead(text_reader_);
		}
		return i;
	}

	int ReadParameterSet(int rowid) {
		int i = xmlTextReaderRead(text_reader_);
		while (i > 0) {
			int type = xmlTextReaderNodeType(text_reader_);
			if (type == XML_READER_TYPE_ELEMENT) {
				const xmlChar *local_name = xmlTextReaderConstLocalName(text_reader_);
				if (xmlStrEqual(local_name, BAD_CAST "parameter")) {
					i = ReadParameter(rowid);
					if (i <= 0) return i;
					continue;
				} else {
					cerr << "unknown child of <parameter-set>: " << local_name << endl;
					return -2;
				}
			} else if (type == XML_READER_TYPE_END_ELEMENT) {
				const xmlChar *local_name = xmlTextReaderConstLocalName(text_reader_);
				if (xmlStrEqual(local_name, BAD_CAST "parameter-set")) {
					return 1;
				}
			}
			i = xmlTextReaderRead(text_reader_);
		}
		return i;
	}

	int ReadParameter(int rowid) {
		boost::scoped_ptr<Parameter> parameter;
		int i;
		while ( (i = xmlTextReaderMoveToNextAttribute(text_reader_)) > 0) {
			if (xmlTextReaderIsNamespaceDecl(text_reader_)) continue;

			const xmlChar *local_name = xmlTextReaderConstLocalName(text_reader_);
			if (xmlStrEqual(local_name, BAD_CAST "name")) {
				parameter.reset(new Parameter(xmlTextReaderValue(text_reader_)));
			} else {
				// ignore
			}
		}
		if (!parameter) {
			cerr << "missing name of <parameter>" << endl;
			return -2;
		}
		i = xmlTextReaderRead(text_reader_);
		while (i > 0) {
			int type = xmlTextReaderNodeType(text_reader_);
			if (type == XML_READER_TYPE_ELEMENT) {
				const xmlChar *local_name = xmlTextReaderConstLocalName(text_reader_);
				if (xmlStrEqual(local_name, BAD_CAST "range")) {
					i = ReadRange(rowid, parameter.get());
					if (i <= 0) return i;
					continue;
				}
			} else if (type == XML_READER_TYPE_END_ELEMENT) {
				const xmlChar *local_name = xmlTextReaderConstLocalName(text_reader_);
				if (xmlStrEqual(local_name, BAD_CAST "parameter")) {
					return xmlTextReaderRead(text_reader_);
				}
			}
			i = xmlTextReaderRead(text_reader_);
		}
		return i;
	}

	int ReadRange(int rowid, Parameter *parameter) {
		boost::scoped_ptr<Range> range(new Range);
		int i;
		while ( (i = xmlTextReaderMoveToNextAttribute(text_reader_)) > 0) {
			if (xmlTextReaderIsNamespaceDecl(text_reader_)) continue;

			const xmlChar *local_name = xmlTextReaderConstLocalName(text_reader_);
			if (xmlStrEqual(local_name, BAD_CAST "type")) {
				const xmlChar *type = xmlTextReaderConstValue(text_reader_);
				if (xmlStrEqual(type, BAD_CAST "interval")) {
					range->set_type(Range::kInterval);
				} else if (xmlStrEqual(type, BAD_CAST "enum")) {
					range->set_type(Range::kEnum);
				} else {
					cerr << "unkown type of <range>: " << type << endl;
					return -2;
				}
			} else if (xmlStrEqual(local_name, BAD_CAST "lower")) {
				boost::rational<long> lower;
				bool b = base::Rational<long>::FromString((const char *)xmlTextReaderConstValue(text_reader_), lower);
				if (!b) {
					cerr << "failed to parse lower" << endl;
					return -2;
				}
				range->set_lower(lower);
			} else if (xmlStrEqual(local_name, BAD_CAST "upper")) {
				boost::rational<long> upper;
				bool b = base::Rational<long>::FromString((const char *)xmlTextReaderConstValue(text_reader_), upper);
				if (!b) {
					cerr << "failed to parse upper" << endl;
					return -2;
				}
				range->set_upper(upper);
			} else if (xmlStrEqual(local_name, BAD_CAST "step")) {
				boost::rational<long> step;
				bool b = base::Rational<long>::FromString((const char *)xmlTextReaderConstValue(text_reader_), step);
				if (!b) {
					cerr << "failed to parse step" << endl;
					return -2;
				}
				range->set_step(step);
			} else {
				cerr << "unknown attribute of <range>: " << local_name << endl;
				return -2;
			}
		}
		if (range->type() == Range::kUnspecified) {
			cerr << "missing type of <range>" << endl;
			return -2;
		}

		if (!range->IsValid()) return -2;

		sqlite3_stmt *stmt;
		char query[1024];
		sprintf(query, "INSERT INTO db%d.phsp_parameters VALUES (?, ?)", rowid);
		int e = sqlite3_prepare_v2(db_, query, -1, &stmt, NULL);
		if (e != SQLITE_OK) {
			cerr << "failed to prepare statement: "
				 << e << " (" << __FILE__ << ":" << __LINE__ << ")"
				 << endl;
			return -2;
		}
		e = sqlite3_bind_text(stmt, 1, (const char *)parameter->name(), -1, SQLITE_STATIC);
		if (e != SQLITE_OK) {
			cerr << "failed to bind parameter: " << e << endl;
			return -2;
		}

		if (range->type() == Range::kInterval) {
			std::ostringstream oss;
			boost::rational<long> d = range->lower();
			oss << boost::rational_cast<double>(d);
			for (;;) {
				d += range->step();
				if (d > range->upper()) break;
				oss << ',' << boost::rational_cast<double>(d);
			}
			string s(oss.str());
			size_t len = s.size();
			char *p = static_cast<char *>(malloc(len));
			if (!p) {
				cerr << "failed to malloc: " << len << endl;
				return -2;
			}
			memcpy(p, s.c_str(), len);
			e = sqlite3_bind_text(stmt, 2, p, len, free);
			if (e != SQLITE_OK) {
				cerr << "failed to bind parameter: " << e << endl;
				return -2;
			}
		} else { // enum
			i = xmlTextReaderMoveToElement(text_reader_);
			if (i < 0) return i;
			e = sqlite3_bind_text(stmt, 2, (const char *)xmlTextReaderReadString(text_reader_), -1, xmlFree);
			if (e != SQLITE_OK) {
				cerr << "failed to bind parameter: " << e << endl;
				return -2;
			}
		}
		e = sqlite3_step(stmt);
		if (e != SQLITE_DONE) {
			cerr << "failed to step statement: " << e << endl;
			return -2;
		}
		sqlite3_finalize(stmt);

		return xmlTextReaderRead(text_reader_);
	}

	int ReadTargetSet(int rowid) {
		int i = xmlTextReaderRead(text_reader_);
		while (i > 0) {
			int type = xmlTextReaderNodeType(text_reader_);
			if (type == XML_READER_TYPE_ELEMENT) {
				const xmlChar *local_name = xmlTextReaderConstLocalName(text_reader_);
				if (xmlStrEqual(local_name, BAD_CAST "target")) {
					i = ReadTarget(rowid);
					if (i <= 0) return i;
					continue;
				} else {
					cerr << "unknown child of <target-set>: " << local_name << endl;
					return -2;
				}
			} else if (type == XML_READER_TYPE_END_ELEMENT) {
				const xmlChar *local_name = xmlTextReaderConstLocalName(text_reader_);
				if (xmlStrEqual(local_name, BAD_CAST "target-set")) {
					return 1;
				}
			}
			i = xmlTextReaderRead(text_reader_);
		}
		return i;
	}

	int ReadTarget(int rowid) {
		std::ostringstream oss;
		boost::scoped_ptr<Target> target(new Target);

		int i;
		while ( (i = xmlTextReaderMoveToNextAttribute(text_reader_)) > 0) {
			if (xmlTextReaderIsNamespaceDecl(text_reader_)) continue;

			const xmlChar *local_name = xmlTextReaderConstLocalName(text_reader_);
			if (xmlStrEqual(local_name, BAD_CAST "module-id")) {
				target->set_uuid(xmlTextReaderValue(text_reader_));
			} else if (xmlStrEqual(local_name, BAD_CAST "physical-quantity-id")) {
				int id = atoi((const char *)xmlTextReaderConstValue(text_reader_));
				if (id <= 0) {
					cerr << "invalid physical-quantity-id: " << id << endl;
					return -2;
				}
				target->set_physical_quantity_id(id);
			} else if (xmlStrEqual(local_name, BAD_CAST "species-id")) {
				target->set_species_id(xmlTextReaderValue(text_reader_));
			} else if (xmlStrEqual(local_name, BAD_CAST "parameter-id")) {
				target->set_parameter_id(xmlTextReaderValue(text_reader_));
			} else if (xmlStrEqual(local_name, BAD_CAST "reaction-id")) {
				target->set_reaction_id(xmlTextReaderValue(text_reader_));
			} else {
				cerr << "unknown attribute of <target>: " << local_name << endl;
				return -2;
			}
		}
		if (target->uuid()) {
			if (target->physical_quantity_id() == 0) {
				cerr << "missing physical-quantity-id of <target>" << endl;
				return -2;
			}
		} else {
			if ( !target->species_id() &&
				 !target->parameter_id() &&
				 !target->reaction_id() ) {
				cerr << "missing species-id/parameter-id/reaction-id of <target>" << endl;
				return -2;
			}
		}

		i = xmlTextReaderRead(text_reader_);
		while (i > 0) {
			int type = xmlTextReaderNodeType(text_reader_);
			if (type == XML_READER_TYPE_ELEMENT) {
				const xmlChar *local_name = xmlTextReaderConstLocalName(text_reader_);
				if (xmlStrEqual(local_name, BAD_CAST "value")) {
					i = ReadValue(&oss);
					if (i <= 0) return i;
					continue;
				} else {
					cerr << "unknown child element of <target>: " << local_name << endl;
					return -2;
				}
			} else if (type == XML_READER_TYPE_END_ELEMENT) {
				const xmlChar *local_name = xmlTextReaderConstLocalName(text_reader_);
				if (xmlStrEqual(local_name, BAD_CAST "target")) {
					string s(oss.str());
					if (s.empty()) {
						cerr << "missing value of <target>" << endl;
						return -2;
					} else {
						char query[1024];
						sprintf(query, "INSERT INTO db%d.phsp_targets values (?, ?, ?)", rowid);
						sqlite3_stmt *stmt;
						int e = sqlite3_prepare_v2(db_, query, -1, &stmt, NULL);
						if (e != SQLITE_OK) {
							cerr << "failed to prepare statement: "
								 << e << " (" << __FILE__ << ":" << __LINE__ << ")"
								 << endl;
							return -2;
						}
						if (target->uuid()) { // PHML
							e = sqlite3_bind_text(stmt, 1, (const char *)target->uuid(), -1, SQLITE_STATIC);
							if (e != SQLITE_OK) {
								cerr << "failed to bind parameter: " << e << endl;
								return -2;
							}
							e = sqlite3_bind_int(stmt, 2, target->physical_quantity_id());
							if (e != SQLITE_OK) {
								cerr << "failed to bind parameter: " << e << endl;
								return -2;
							}
						} else { // SBML
							e = sqlite3_bind_text(stmt, 1, "00000000-0000-0000-0000-000000000000", -1, SQLITE_STATIC);
							if (e != SQLITE_OK) {
								cerr << "failed to bind parameter: " << e << endl;
								return -2;
							}
							const char *id = NULL;
							if (target->species_id()) id = (const char *)target->species_id();
							if (target->parameter_id()) id = (const char *)target->parameter_id();
							if (target->reaction_id()) id = (const char *)target->reaction_id();
							assert(id);
							size_t len = strlen(id);
							char *buf = (char *)malloc(len + 6);
							if (!buf) {
								cerr << "failed to malloc" << endl;
								return -2;
							}
							sprintf(buf, "sbml:%s", id);
							e = sqlite3_bind_text(stmt, 2, buf, -1, free);
							if (e != SQLITE_OK) {
								cerr << "failed to bind parameter: " << e << endl;
								return -2;
							}
						}
						e = sqlite3_bind_text(stmt, 3, s.c_str(), -1, SQLITE_STATIC);
						if (e != SQLITE_OK) {
							cerr << "failed to bind parameter: " << e << endl;
							return -2;
						}
						e = sqlite3_step(stmt);
						if (e != SQLITE_DONE) {
							cerr << "failed to step statement: " << e << endl;
							return -2;
						}
						sqlite3_finalize(stmt);
					}
					return xmlTextReaderRead(text_reader_);
				}
			}
			i = xmlTextReaderRead(text_reader_);
		}
		return i;
	}

	class Handler {
	public:
		Handler() : count_() {}

		int Handle(int i) {
			if (count_++) {
				cerr << "two or more elements in <math>" << endl;
				return -2;
			}
			return i;
		}

	private:
		int count_;
	};

	int ReadValue(std::ostringstream *oss) {
		int i = xmlTextReaderRead(text_reader_);
		while (i > 0) {
			int type = xmlTextReaderNodeType(text_reader_);
			if (type == XML_READER_TYPE_ELEMENT) {
				const xmlChar *local_name = xmlTextReaderConstLocalName(text_reader_);
				if (xmlStrEqual(local_name, BAD_CAST "math")) {
					boost::scoped_ptr<Handler> handler(new Handler);
					boost::scoped_ptr<mathml::MathDumper> math_dumper(new mathml::MathDumper(text_reader_, oss));
					i = math_dumper->Read(handler.get());
					if (i <= 0) return i;
					continue;
				} else {
					cerr << "unknown child element of <value>: " << local_name << endl;
					return -2;
				}
			} else if (type == XML_READER_TYPE_END_ELEMENT) {
				const xmlChar *local_name = xmlTextReaderConstLocalName(text_reader_);
				if (xmlStrEqual(local_name, BAD_CAST "value")) {
					return xmlTextReaderRead(text_reader_);
				}
			}
			i = xmlTextReaderRead(text_reader_);
		}
		return i;
	}

	xmlTextReaderPtr &text_reader_;
	sqlite3 *db_;
};

}

bool Read(const char *phsp_file, sqlite3 *db)
{
	boost::filesystem::path pp = GetPathFromUtf8(phsp_file);
	string pp_s = pp.string();

	xmlTextReaderPtr text_reader = xmlReaderForFile(pp_s.c_str(), NULL, 0);
	if (!text_reader) {
		cerr << "could not read " << phsp_file << endl;
		return false;
	}

	boost::scoped_ptr<Reader> reader(new Reader(text_reader, db));
	boost::filesystem::path cp = pp.parent_path();
	if (reader->Read(cp) < 0) return false;

	return true;
}

}
