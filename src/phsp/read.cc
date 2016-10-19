/* -*- Mode: C++; tab-width: 4; indent-tabs-mode: t; c-basic-offset: 4 -*- vim:set ts=4 sw=4 sts=4 noet: */
#ifdef HAVE_CONFIG_H
#include "config.h"
#endif

#include "phsp.h"

#include <cstdio>
#include <cstdlib>
#include <cstring>
#include <iostream>
#include <map>
#include <memory>
#include <sstream>
#include <string>

#define BOOST_FILESYSTEM_NO_DEPRECATED
#include <boost/filesystem.hpp>
#include <boost/uuid/nil_generator.hpp>
#include <boost/uuid/string_generator.hpp>

#include <libxml/xmlreader.h>

#include "base/rational.h"
#include "db/query.h"
#include "mathml/math_dumper.h"
#include "sqlite3.h"
#include "utf8path.h"

namespace flint {
namespace phsp {

namespace {

class Model {
public:
	Model(const Model &) = delete;
	Model &operator=(const Model &) = delete;

	Model() : format_(nullptr), iref_(nullptr) {}

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

class Parameter {
public:
	Parameter(const Parameter &) = delete;
	Parameter &operator=(const Parameter &) = delete;

	explicit Parameter(xmlChar *name) : name_(name) {}

	~Parameter() {
		if (name_) xmlFree(name_);
	}

	const xmlChar *name() const {return name_;}

private:
	xmlChar *name_;
};

class Range {
public:
	enum Type {
		kUnspecified,
		kInterval,
		kEnum
	};

	Range(const Range &) = delete;
	Range &operator=(const Range &) = delete;

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
			std::cerr << "type is unspecified" << std::endl;
			return false;
		}
		if (type_ == kInterval) {
			if (upper_ < lower_) {
				std::cerr << "the upper value ("
					 << upper_
					 << ") is smaller than the lower one ("
					 << lower_
					 << ")"
					 << std::endl;
				return false;
			}
			if (step_ <= 0) {
				std::cerr << "the step value is non-positive: " << step_ << std::endl;
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

class Target {
public:
	Target(const Target &) = delete;
	Target &operator=(const Target &) = delete;

	Target()
		: uuid_(nullptr),
		  physical_quantity_id_(),
		  species_id_(nullptr),
		  parameter_id_(nullptr),
		  reaction_id_(nullptr)
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

	boost::uuids::uuid GetUuid() const {
		assert(uuid_);
		boost::uuids::string_generator gen;
		return gen(reinterpret_cast<const char *>(uuid_));
	}

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
				if (xmlStrEqual(local_name, reinterpret_cast<const xmlChar *>("phsp"))) {
					// skip the top element
				} else if (xmlStrEqual(local_name, reinterpret_cast<const xmlChar *>("model"))) {
					i = ReadModel(cp);
					if (i <= 0) return i;
					continue;
				} else {
					std::cerr << "unknown child of <phsp>: " << local_name << std::endl;
					return -2;
				}
			} else if (type == XML_READER_TYPE_END_ELEMENT) {
				const xmlChar *local_name = xmlTextReaderConstLocalName(text_reader_);
				if (xmlStrEqual(local_name, reinterpret_cast<const xmlChar *>("phsp"))) {
					return 1;
				}
			}
			i = xmlTextReaderRead(text_reader_);
		}
		return i;
	}

private:
	int ReadModel(const boost::filesystem::path &cp) {
		std::unique_ptr<Model> model(new Model);
		int i;
		while ( (i = xmlTextReaderMoveToNextAttribute(text_reader_)) > 0) {
			if (xmlTextReaderIsNamespaceDecl(text_reader_)) continue;

			const xmlChar *local_name = xmlTextReaderConstLocalName(text_reader_);
			if (xmlStrEqual(local_name, reinterpret_cast<const xmlChar *>("format"))) {
				model->set_format(xmlTextReaderValue(text_reader_));
				if ( !xmlStrEqual(model->format(), reinterpret_cast<const xmlChar *>("phml")) &&
					 !xmlStrEqual(model->format(), reinterpret_cast<const xmlChar *>("sbml")) ) {
					std::cerr << "unknown format of <model>: " << reinterpret_cast<const char *>(model->format()) << std::endl;
					return -2;
				}
			} else if (xmlStrEqual(local_name, reinterpret_cast<const xmlChar *>("iref"))) {
				model->set_iref(xmlTextReaderValue(text_reader_));
			}
		}
		if (!model->format()) {
			std::cerr << "missing format of <model>" << std::endl;
			return -2;
		}
		if (!model->iref()) {
			std::cerr << "missing iref of <model>" << std::endl;
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
							   -1, &stmt, nullptr);
		if (e != SQLITE_OK) {
			std::cerr << "failed to prepare statement: "
				 << e << " (" << __FILE__ << ":" << __LINE__ << ")"
				 << std::endl;
			return -2;
		}
		e = sqlite3_bind_text(stmt, 1, reinterpret_cast<const char *>(model->iref()), -1, SQLITE_STATIC);
		if (e != SQLITE_OK) {
			std::cerr << "failed to bind parameter: " << e << std::endl;
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
					if (xmlStrEqual(local_name, reinterpret_cast<const xmlChar *>("model"))) {
						return xmlTextReaderRead(text_reader_);
					}
				}
				i = xmlTextReaderRead(text_reader_);
			}
			return i;
		}
		if (e != SQLITE_ROW) {
			std::cerr << "failed to find a corresponding model: "
					  << reinterpret_cast<const char *>(model->iref())
				 << std::endl;
			return -2;
		}
		int rowid = static_cast<int>(sqlite3_column_int64(stmt, 0));
		sqlite3_finalize(stmt);

		// create a task directory
		char dir_path[1024];
		std::sprintf(dir_path, "%d", rowid);
		boost::filesystem::path dp(dir_path);
		if (!boost::filesystem::is_directory(dp) && !boost::filesystem::create_directory(dp)) {
			std::cerr << "failed to create a directory: "
				 << dir_path
				 << std::endl;
			return -2;
		}

		boost::filesystem::path mp = GetPathFromUtf8(reinterpret_cast<const char *>(model->iref()));
		boost::filesystem::path amp = boost::filesystem::absolute(mp, cp);
		std::unique_ptr<char[]> utf8amp(GetUtf8FromPath(amp));

		// update the model entry
		e = sqlite3_prepare_v2(db_, "UPDATE models SET absolute_path = ? WHERE rowid = ?",
							   -1, &stmt, nullptr);
		if (e != SQLITE_OK) {
			std::cerr << "failed to prepare statement: "
				 << e << " (" << __FILE__ << ":" << __LINE__ << ")"
				 << std::endl;
			return -2;
		}
		e = sqlite3_bind_text(stmt, 1, utf8amp.get(), -1, SQLITE_STATIC);
		if (e != SQLITE_OK) {
			std::cerr << "failed to bind absolute_path: " << e << std::endl;
			return -2;
		}
		e = sqlite3_bind_int64(stmt, 2, rowid);
		if (e != SQLITE_OK) {
			std::cerr << "failed to bind rowid: " << e << std::endl;
			return -2;
		}
		e = sqlite3_step(stmt);
		if (e != SQLITE_DONE) {
			std::cerr << "failed to step statement: " << e << std::endl;
			return -2;
		}
		sqlite3_finalize(stmt);

		// attach a task database for the model
		char query[1024];
		std::sprintf(query, "ATTACH DATABASE '%d/task.db' AS 'db%d'", rowid, rowid);
		char *em;
		e = sqlite3_exec(db_, query, nullptr, nullptr, &em);
		if (e != SQLITE_OK) {
			std::cerr << "failed to attach database: " << e
				 << ": " << em << std::endl;
			sqlite3_free(em);
			return -2;
		}

		// begin a transaction per attached session
		if (!BeginTransaction(db_))
			return -2;

		char table_name[64]; // long enough
		// save model's format
		std::sprintf(table_name, "db%d.model", rowid);
		if (!CreateTable(db_, table_name, "(format TEXT)"))
			return -2;
		std::sprintf(query, "INSERT INTO db%d.model VALUES (?)", rowid);
		e = sqlite3_prepare_v2(db_, query, -1, &stmt, nullptr);
		if (e != SQLITE_OK) {
			std::cerr << "failed to prepare statement: "
				 << e << " (" << __FILE__ << ":" << __LINE__ << ")"
				 << std::endl;
			return -2;
		}
		e = sqlite3_bind_text(stmt, 1, reinterpret_cast<const char *>(model->format()), -1, SQLITE_STATIC);
		if (e != SQLITE_OK) {
			std::cerr << "failed to bind parameter: " << e << std::endl;
			return -2;
		}
		e = sqlite3_step(stmt);
		if (e != SQLITE_DONE) {
			std::cerr << "failed to step statement: " << e << std::endl;
			return -2;
		}
		sqlite3_finalize(stmt);

		// prepare schemas
		std::sprintf(table_name, "db%d.phsp_parameters", rowid);
		if (!CreateTable(db_, table_name, "(name TEXT, range TEXT)"))
			return -2;
		std::sprintf(table_name, "db%d.phsp_targets", rowid);
		if (!CreateTable(db_, table_name, "(uuid BLOB, id TEXT, math TEXT)"))
			return -2;

		i = xmlTextReaderRead(text_reader_);
		while (i > 0) {
			int type = xmlTextReaderNodeType(text_reader_);
			if (type == XML_READER_TYPE_ELEMENT) {
				const xmlChar *local_name = xmlTextReaderConstLocalName(text_reader_);
				if (xmlStrEqual(local_name, reinterpret_cast<const xmlChar *>("parameter-set"))) {
					i = ReadParameterSet(rowid);
					if (i <= 0) return i;
					continue;
				} else if (xmlStrEqual(local_name, reinterpret_cast<const xmlChar *>("target-set"))) {
					i = ReadTargetSet(rowid);
					if (i <= 0) return i;
					continue;
				}
			} else if (type == XML_READER_TYPE_END_ELEMENT) {
				const xmlChar *local_name = xmlTextReaderConstLocalName(text_reader_);
				if (xmlStrEqual(local_name, reinterpret_cast<const xmlChar *>("model"))) {
					// commit the transaction per attached session
					if (!CommitTransaction(db_))
						return -2;
					// detach the database after the transaction
					std::sprintf(query, "DETACH DATABASE 'db%d'", rowid);
					e = sqlite3_exec(db_, query, nullptr, nullptr, &em);
					if (e != SQLITE_OK) {
						std::cerr << "failed to detach database: " << e
							 << ": " << em << std::endl;
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
				if (xmlStrEqual(local_name, reinterpret_cast<const xmlChar *>("parameter"))) {
					i = ReadParameter(rowid);
					if (i <= 0) return i;
					continue;
				} else {
					std::cerr << "unknown child of <parameter-set>: " << local_name << std::endl;
					return -2;
				}
			} else if (type == XML_READER_TYPE_END_ELEMENT) {
				const xmlChar *local_name = xmlTextReaderConstLocalName(text_reader_);
				if (xmlStrEqual(local_name, reinterpret_cast<const xmlChar *>("parameter-set"))) {
					return 1;
				}
			}
			i = xmlTextReaderRead(text_reader_);
		}
		return i;
	}

	int ReadParameter(int rowid) {
		std::unique_ptr<Parameter> parameter;
		int i;
		while ( (i = xmlTextReaderMoveToNextAttribute(text_reader_)) > 0) {
			if (xmlTextReaderIsNamespaceDecl(text_reader_)) continue;

			const xmlChar *local_name = xmlTextReaderConstLocalName(text_reader_);
			if (xmlStrEqual(local_name, reinterpret_cast<const xmlChar *>("name"))) {
				parameter.reset(new Parameter(xmlTextReaderValue(text_reader_)));
			} else {
				// ignore
			}
		}
		if (!parameter) {
			std::cerr << "missing name of <parameter>" << std::endl;
			return -2;
		}
		i = xmlTextReaderRead(text_reader_);
		while (i > 0) {
			int type = xmlTextReaderNodeType(text_reader_);
			if (type == XML_READER_TYPE_ELEMENT) {
				const xmlChar *local_name = xmlTextReaderConstLocalName(text_reader_);
				if (xmlStrEqual(local_name, reinterpret_cast<const xmlChar *>("range"))) {
					i = ReadRange(rowid, parameter.get());
					if (i <= 0) return i;
					continue;
				}
			} else if (type == XML_READER_TYPE_END_ELEMENT) {
				const xmlChar *local_name = xmlTextReaderConstLocalName(text_reader_);
				if (xmlStrEqual(local_name, reinterpret_cast<const xmlChar *>("parameter"))) {
					return xmlTextReaderRead(text_reader_);
				}
			}
			i = xmlTextReaderRead(text_reader_);
		}
		return i;
	}

	int ReadRange(int rowid, Parameter *parameter) {
		std::unique_ptr<Range> range(new Range);
		int i;
		while ( (i = xmlTextReaderMoveToNextAttribute(text_reader_)) > 0) {
			if (xmlTextReaderIsNamespaceDecl(text_reader_)) continue;

			const xmlChar *local_name = xmlTextReaderConstLocalName(text_reader_);
			if (xmlStrEqual(local_name, reinterpret_cast<const xmlChar *>("type"))) {
				const xmlChar *type = xmlTextReaderConstValue(text_reader_);
				if (xmlStrEqual(type, reinterpret_cast<const xmlChar *>("interval"))) {
					range->set_type(Range::kInterval);
				} else if (xmlStrEqual(type, reinterpret_cast<const xmlChar *>("enum"))) {
					range->set_type(Range::kEnum);
				} else {
					std::cerr << "unkown type of <range>: " << type << std::endl;
					return -2;
				}
			} else if (xmlStrEqual(local_name, reinterpret_cast<const xmlChar *>("lower"))) {
				boost::rational<long> lower;
				bool b = base::Rational<long>::FromString(reinterpret_cast<const char *>(xmlTextReaderConstValue(text_reader_)), lower);
				if (!b) {
					std::cerr << "failed to parse lower" << std::endl;
					return -2;
				}
				range->set_lower(lower);
			} else if (xmlStrEqual(local_name, reinterpret_cast<const xmlChar *>("upper"))) {
				boost::rational<long> upper;
				bool b = base::Rational<long>::FromString(reinterpret_cast<const char *>(xmlTextReaderConstValue(text_reader_)), upper);
				if (!b) {
					std::cerr << "failed to parse upper" << std::endl;
					return -2;
				}
				range->set_upper(upper);
			} else if (xmlStrEqual(local_name, reinterpret_cast<const xmlChar *>("step"))) {
				boost::rational<long> step;
				bool b = base::Rational<long>::FromString(reinterpret_cast<const char *>(xmlTextReaderConstValue(text_reader_)), step);
				if (!b) {
					std::cerr << "failed to parse step" << std::endl;
					return -2;
				}
				range->set_step(step);
			} else {
				std::cerr << "unknown attribute of <range>: " << local_name << std::endl;
				return -2;
			}
		}
		if (range->type() == Range::kUnspecified) {
			std::cerr << "missing type of <range>" << std::endl;
			return -2;
		}

		if (!range->IsValid()) return -2;

		sqlite3_stmt *stmt;
		char query[1024];
		std::sprintf(query, "INSERT INTO db%d.phsp_parameters VALUES (?, ?)", rowid);
		int e = sqlite3_prepare_v2(db_, query, -1, &stmt, nullptr);
		if (e != SQLITE_OK) {
			std::cerr << "failed to prepare statement: "
				 << e << " (" << __FILE__ << ":" << __LINE__ << ")"
				 << std::endl;
			return -2;
		}
		e = sqlite3_bind_text(stmt, 1, reinterpret_cast<const char *>(parameter->name()), -1, SQLITE_STATIC);
		if (e != SQLITE_OK) {
			std::cerr << "failed to bind parameter: " << e << std::endl;
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
			std::string s(oss.str());
			size_t len = s.size();
			char *p = static_cast<char *>(malloc(len));
			if (!p) {
				std::cerr << "failed to malloc: " << len << std::endl;
				return -2;
			}
			memcpy(p, s.c_str(), len);
			e = sqlite3_bind_text(stmt, 2, p, len, free);
			if (e != SQLITE_OK) {
				std::cerr << "failed to bind parameter: " << e << std::endl;
				return -2;
			}
		} else { // enum
			i = xmlTextReaderMoveToElement(text_reader_);
			if (i < 0) return i;
			e = sqlite3_bind_text(stmt, 2, reinterpret_cast<const char *>(xmlTextReaderReadString(text_reader_)), -1, xmlFree);
			if (e != SQLITE_OK) {
				std::cerr << "failed to bind parameter: " << e << std::endl;
				return -2;
			}
		}
		e = sqlite3_step(stmt);
		if (e != SQLITE_DONE) {
			std::cerr << "failed to step statement: " << e << std::endl;
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
				if (xmlStrEqual(local_name, reinterpret_cast<const xmlChar *>("target"))) {
					i = ReadTarget(rowid);
					if (i <= 0) return i;
					continue;
				} else {
					std::cerr << "unknown child of <target-set>: " << local_name << std::endl;
					return -2;
				}
			} else if (type == XML_READER_TYPE_END_ELEMENT) {
				const xmlChar *local_name = xmlTextReaderConstLocalName(text_reader_);
				if (xmlStrEqual(local_name, reinterpret_cast<const xmlChar *>("target-set"))) {
					return 1;
				}
			}
			i = xmlTextReaderRead(text_reader_);
		}
		return i;
	}

	int ReadTarget(int rowid) {
		std::ostringstream oss;
		std::unique_ptr<Target> target(new Target);

		int i;
		while ( (i = xmlTextReaderMoveToNextAttribute(text_reader_)) > 0) {
			if (xmlTextReaderIsNamespaceDecl(text_reader_)) continue;

			const xmlChar *local_name = xmlTextReaderConstLocalName(text_reader_);
			if (xmlStrEqual(local_name, reinterpret_cast<const xmlChar *>("module-id"))) {
				target->set_uuid(xmlTextReaderValue(text_reader_));
			} else if (xmlStrEqual(local_name, reinterpret_cast<const xmlChar *>("physical-quantity-id"))) {
				int id = atoi(reinterpret_cast<const char *>(xmlTextReaderConstValue(text_reader_)));
				if (id <= 0) {
					std::cerr << "invalid physical-quantity-id: " << id << std::endl;
					return -2;
				}
				target->set_physical_quantity_id(id);
			} else if (xmlStrEqual(local_name, reinterpret_cast<const xmlChar *>("species-id"))) {
				target->set_species_id(xmlTextReaderValue(text_reader_));
			} else if (xmlStrEqual(local_name, reinterpret_cast<const xmlChar *>("parameter-id"))) {
				target->set_parameter_id(xmlTextReaderValue(text_reader_));
			} else if (xmlStrEqual(local_name, reinterpret_cast<const xmlChar *>("reaction-id"))) {
				target->set_reaction_id(xmlTextReaderValue(text_reader_));
			} else {
				std::cerr << "unknown attribute of <target>: " << local_name << std::endl;
				return -2;
			}
		}
		if (target->uuid()) {
			if (target->physical_quantity_id() == 0) {
				std::cerr << "missing physical-quantity-id of <target>" << std::endl;
				return -2;
			}
		} else {
			if ( !target->species_id() &&
				 !target->parameter_id() &&
				 !target->reaction_id() ) {
				std::cerr << "missing species-id/parameter-id/reaction-id of <target>" << std::endl;
				return -2;
			}
		}

		i = xmlTextReaderRead(text_reader_);
		while (i > 0) {
			int type = xmlTextReaderNodeType(text_reader_);
			if (type == XML_READER_TYPE_ELEMENT) {
				const xmlChar *local_name = xmlTextReaderConstLocalName(text_reader_);
				if (xmlStrEqual(local_name, reinterpret_cast<const xmlChar *>("value"))) {
					i = ReadValue(&oss);
					if (i <= 0) return i;
					continue;
				} else {
					std::cerr << "unknown child element of <target>: " << local_name << std::endl;
					return -2;
				}
			} else if (type == XML_READER_TYPE_END_ELEMENT) {
				const xmlChar *local_name = xmlTextReaderConstLocalName(text_reader_);
				if (xmlStrEqual(local_name, reinterpret_cast<const xmlChar *>("target"))) {
					std::string s(oss.str());
					if (s.empty()) {
						std::cerr << "missing value of <target>" << std::endl;
						return -2;
					} else {
						char query[1024];
						std::sprintf(query, "INSERT INTO db%d.phsp_targets values (?, ?, ?)", rowid);
						sqlite3_stmt *stmt;
						int e = sqlite3_prepare_v2(db_, query, -1, &stmt, nullptr);
						if (e != SQLITE_OK) {
							std::cerr << "failed to prepare statement: "
								 << e << " (" << __FILE__ << ":" << __LINE__ << ")"
								 << std::endl;
							return -2;
						}
						boost::uuids::uuid u; // to be alive until sqlite3_step()
						if (target->uuid()) { // PHML
							u = target->GetUuid();
							e = sqlite3_bind_blob(stmt, 1, &u, u.size(), SQLITE_STATIC);
							if (e != SQLITE_OK) {
								std::cerr << "failed to bind uuid: " << e << std::endl;
								return -2;
							}
							e = sqlite3_bind_int(stmt, 2, target->physical_quantity_id());
							if (e != SQLITE_OK) {
								std::cerr << "failed to bind parameter: " << e << std::endl;
								return -2;
							}
						} else { // SBML
							u = boost::uuids::nil_uuid();
							e = sqlite3_bind_blob(stmt, 1, &u, u.size(), SQLITE_STATIC);
							if (e != SQLITE_OK) {
								std::cerr << "failed to bind uuid: " << e << std::endl;
								return -2;
							}
							const char *id = nullptr;
							if (target->species_id()) id = reinterpret_cast<const char *>(target->species_id());
							if (target->parameter_id()) id = reinterpret_cast<const char *>(target->parameter_id());
							if (target->reaction_id()) id = reinterpret_cast<const char *>(target->reaction_id());
							assert(id);
							size_t len = strlen(id);
							char *buf = static_cast<char *>(malloc(len + 6));
							if (!buf) {
								std::cerr << "failed to malloc" << std::endl;
								return -2;
							}
							std::sprintf(buf, "sbml:%s", id);
							e = sqlite3_bind_text(stmt, 2, buf, -1, free);
							if (e != SQLITE_OK) {
								std::cerr << "failed to bind parameter: " << e << std::endl;
								return -2;
							}
						}
						e = sqlite3_bind_text(stmt, 3, s.c_str(), -1, SQLITE_STATIC);
						if (e != SQLITE_OK) {
							std::cerr << "failed to bind parameter: " << e << std::endl;
							return -2;
						}
						e = sqlite3_step(stmt);
						if (e != SQLITE_DONE) {
							std::cerr << "failed to step statement: " << e << std::endl;
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
				std::cerr << "two or more elements in <math>" << std::endl;
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
				if (xmlStrEqual(local_name, reinterpret_cast<const xmlChar *>("math"))) {
					std::unique_ptr<Handler> handler(new Handler);
					std::unique_ptr<mathml::MathDumper> math_dumper(new mathml::MathDumper(text_reader_, oss));
					i = math_dumper->Read(handler.get());
					if (i <= 0) return i;
					continue;
				} else {
					std::cerr << "unknown child element of <value>: " << local_name << std::endl;
					return -2;
				}
			} else if (type == XML_READER_TYPE_END_ELEMENT) {
				const xmlChar *local_name = xmlTextReaderConstLocalName(text_reader_);
				if (xmlStrEqual(local_name, reinterpret_cast<const xmlChar *>("value"))) {
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
	std::string pp_s = pp.string();

	xmlTextReaderPtr text_reader = xmlReaderForFile(pp_s.c_str(), nullptr, 0);
	if (!text_reader) {
		std::cerr << "could not read " << phsp_file << std::endl;
		return false;
	}

	std::unique_ptr<Reader> reader(new Reader(text_reader, db));
	boost::filesystem::path cp = pp.parent_path();
	if (reader->Read(cp) < 0) return false;

	return true;
}

}
}
