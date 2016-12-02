/* -*- Mode: C++; tab-width: 4; indent-tabs-mode: t; c-basic-offset: 4 -*- vim:set ts=4 sw=4 sts=4 noet: */
#ifdef HAVE_CONFIG_H
#include "config.h"
#endif

#include "phsp.h"

#include <cassert>
#include <cerrno>
#include <cmath>
#include <cstdio>
#include <cstdlib>
#include <cstring>
#include <iostream>
#include <map>
#include <memory>
#include <random>
#include <sstream>
#include <string>

#define BOOST_FILESYSTEM_NO_DEPRECATED
#include <boost/filesystem.hpp>
#include <boost/uuid/nil_generator.hpp>
#include <boost/uuid/string_generator.hpp>

#include <libxml/xmlreader.h>

#include "base/rational.h"
#include "db/query.h"
#include "db/statement-driver.h"
#include "db/utility.h"
#include "flint/numeric.h"
#include "mathml/math_dumper.h"
#include "phsp/sample-builder.h"
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
	enum class Type {
		kSequence,
		kRandom
	};

	Parameter(const Parameter &) = delete;
	Parameter &operator=(const Parameter &) = delete;

	explicit Parameter(Type type)
		: type_(type)
		, name_(nullptr)
	{}

	~Parameter() {
		xmlFree(name_);
	}

	Type type() const {return type_;}
	const xmlChar *name() const {return name_;}
	void set_type(Type type) {type_ = type;}
	void set_name(xmlChar *name) {name_ = name;}

	void PrintType(std::ostream &os) {
		switch (type_) {
		case Type::kSequence:
			os << "sequence";
			break;
		case Type::kRandom:
			os << "random";
			break;
		}
	}

private:
	Type type_;
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

bool ParseEnum(const char *text, std::vector<double> *values)
{
	auto len = std::strlen(text);
	std::unique_ptr<char[]> buf(new char[len+1]());
	std::strcpy(buf.get(), text);
	for (size_t i=0;i<len;i++) {
		if (buf[i] == ',')
			buf[i] = ' ';
	}
	char *p = buf.get();
	while (*p) {
		errno = 0;
		char *q;
		auto d = std::strtod(p, &q);
		if (d == 0) {
			if (p == q) { // no conversion
				// done
				return true;
			}
			if (errno == ERANGE) {
				std::cerr << "failed to convert enum value: "
						  << p << std::endl;
				return false;
			}
		} else if (d == HUGE_VAL || d == -HUGE_VAL) {
			assert(errno == ERANGE);
			std::cerr << "failed to convert enum value: "
					  << p << std::endl;
			return false;
		}
		values->push_back(d);
		p = q;
	}
	return true;
}

bool SaveParameterSamples(int rowid, const SampleElement &se, sqlite3 *db)
{
	std::vector<std::string> names;
	se.GetColumns(&names);

	std::ostringstream oss_c, oss_i;
	oss_c << '(';
	oss_i << "INSERT INTO db" << rowid << ".parameter_samples VALUES (";
	bool first = true;
	for (const auto &name : names) {
		if (first) {
			first = false;
		} else {
			oss_c << ", ";
			oss_i << ", ";
		}
		oss_c << name << " REAL";
		oss_i << '?';
	}
	oss_c << ')';
	oss_i << ')';

	std::string columns = oss_c.str();
	char table_name[64]; // long enough
	std::sprintf(table_name, "db%d.parameter_samples", rowid);
	if (!CreateTable(db, table_name, columns.c_str()))
		return false;

	std::string query = oss_i.str();
	int n_bytes = static_cast<int>(query.size());
	assert(n_bytes > 0);
	sqlite3_stmt *stmt;
	int e = sqlite3_prepare_v2(db, query.c_str(), n_bytes+1, &stmt, nullptr);
	if (e != SQLITE_OK) {
		std::cerr << "failed to prepare statement: "
				  << query << ": "
				  << e << std::endl;
		return false;
	}

	db::StatementDriver job_sd(db, "INSERT INTO jobs VALUES (?, 'pending')");

	size_t count = se.GetCount();
	size_t n_cols = se.GetNumOfCols();
	bool result;
	std::unique_ptr<double[]> vals(new double[n_cols]);
	for (size_t i=0;i<count;i++) {
		se.Fill(i, vals.get());
		for (size_t j=0;j<n_cols;j++) {
			e = sqlite3_bind_double(stmt, j+1, vals[j]);
			if (e != SQLITE_OK) {
				std::cerr << "failed to bind " << names.at(j)
						  << ": " << e << std::endl;
				result = false;
				goto bail;
			}
		}
		e = sqlite3_step(stmt);
		if (e != SQLITE_DONE) {
			std::cerr << "failed to step statement: " << e << std::endl;
			result = false;
			goto bail;
		}
		e = sqlite3_reset(stmt);
		if (e != SQLITE_OK) {
			std::cerr << "failed to reset statement: " << e << std::endl;
			result = false;
			goto bail;
		}

		sqlite3_int64 ps_id = sqlite3_last_insert_rowid(db);
		e = sqlite3_bind_int64(job_sd.stmt(), 1, ps_id);
		if (e != SQLITE_OK) {
			std::cerr << "failed to bind parameter: " << e << std::endl;
			result = false;
			goto bail;
		}
		e = sqlite3_step(job_sd.stmt());
		if (e != SQLITE_DONE) {
			std::cerr << "failed to step statement: " << e << std::endl;
			result = false;
			goto bail;
		}
		e = sqlite3_reset(job_sd.stmt());
		if (e != SQLITE_OK) {
			std::cerr << "failed to reset statement: " << e << std::endl;
			result = false;
			goto bail;
		}
	}
	result = true;
 bail:
	sqlite3_finalize(stmt);
	return result;
}

class Reader {
public:
	Reader(xmlTextReaderPtr &text_reader, sqlite3 *db)
		: text_reader_(text_reader)
		, db_(db)
		, sd_param_()
		, rd_()
		, gen_(rd_())
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
		e = db::PrepareStatement(db_,
								 "SELECT tasks.rowid FROM tasks"
								 " LEFT JOIN models ON tasks.model_id = models.rowid"
								 " WHERE models.model_path = ?"
								 " ORDER BY tasks.rowid DESC",
								 &stmt);
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
		if (mp.empty())
			return -2;
		boost::filesystem::path amp = boost::filesystem::absolute(mp, cp);
		std::unique_ptr<char[]> utf8amp(GetUtf8FromPath(amp));
		if (!utf8amp)
			return -2;

		// update the model entry
		e = db::PrepareStatement(db_, "UPDATE models SET absolute_path = ? WHERE rowid = ?",
								 &stmt);
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
		int n_bytes = std::sprintf(query, "INSERT INTO db%d.model VALUES (?)", rowid);
		assert(n_bytes > 0);
		e = sqlite3_prepare_v2(db_, query, n_bytes+1, &stmt, nullptr);
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
		std::sprintf(table_name, "db%d.jobs", rowid);
		if (!CreateTable(db_, table_name, "(ps_id INTEGER, status TEXT)"))
			return -2;

		// prepare statements for inserting into phsp_parameters
		n_bytes = std::sprintf(query, "INSERT INTO db%d.phsp_parameters VALUES (?, ?)", rowid);
		assert(n_bytes > 0);
		e = sqlite3_prepare_v2(db_, query, n_bytes+1, &stmt, nullptr);
		if (e != SQLITE_OK) {
			std::cerr << "failed to prepare statement: "
				 << e << " (" << __FILE__ << ":" << __LINE__ << ")"
				 << std::endl;
			return -2;
		}
		sd_param_.reset(new db::StatementDriver(stmt));

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
		SampleBuilder sb;
		int i = xmlTextReaderRead(text_reader_);
		while (i > 0) {
			int type = xmlTextReaderNodeType(text_reader_);
			if (type == XML_READER_TYPE_ELEMENT) {
				const xmlChar *local_name = xmlTextReaderConstLocalName(text_reader_);
				if (xmlStrEqual(local_name, reinterpret_cast<const xmlChar *>("parameter"))) {
					i = ReadParameter(&sb);
					if (i <= 0) return i;
					continue;
				} if (xmlStrEqual(local_name, reinterpret_cast<const xmlChar *>("product"))) {
					i = ReadProduct(&sb);
					if (i <= 0)
						return i;
					continue;
				} if (xmlStrEqual(local_name, reinterpret_cast<const xmlChar *>("zip"))) {
					i = ReadZip(&sb);
					if (i <= 0)
						return i;
					continue;
				} else {
					std::cerr << "unknown child of <parameter-set>: " << local_name << std::endl;
					return -2;
				}
			} else if (type == XML_READER_TYPE_END_ELEMENT) {
				const xmlChar *local_name = xmlTextReaderConstLocalName(text_reader_);
				if (xmlStrEqual(local_name, reinterpret_cast<const xmlChar *>("parameter-set"))) {
					std::unique_ptr<SampleElement> se(sb.Build());
					if (!SaveParameterSamples(rowid, *se, db_))
						return -2;
					return 1;
				}
			}
			i = xmlTextReaderRead(text_reader_);
		}
		return i;
	}

	int ReadParameter(SampleBuilder *sb) {
		std::unique_ptr<Parameter> parameter(new Parameter(Parameter::Type::kSequence));
		int i;
		while ( (i = xmlTextReaderMoveToNextAttribute(text_reader_)) > 0) {
			if (xmlTextReaderIsNamespaceDecl(text_reader_)) continue;

			const xmlChar *local_name = xmlTextReaderConstLocalName(text_reader_);
			if (xmlStrEqual(local_name, reinterpret_cast<const xmlChar *>("name"))) {
				if (parameter->name()) {
					std::cerr << "duplicate name of <parameter>" << std::endl;
					return -2;
				}
				parameter->set_name(xmlTextReaderValue(text_reader_));
			} else if (xmlStrEqual(local_name, reinterpret_cast<const xmlChar *>("type"))) {
				const xmlChar *value = xmlTextReaderConstValue(text_reader_);
				if (xmlStrEqual(value, reinterpret_cast<const xmlChar *>("sequence"))) {
					parameter->set_type(Parameter::Type::kSequence);
				} else if (xmlStrEqual(value, reinterpret_cast<const xmlChar *>("random"))) {
					parameter->set_type(Parameter::Type::kRandom);
				} else {
					std::cerr << "unknown type of <parameter>: " << value << std::endl;
					return -2;
				}
			} else {
				// ignore
			}
		}
		if (!parameter->name()) {
			std::cerr << "missing name of <parameter>" << std::endl;
			return -2;
		}
		i = xmlTextReaderRead(text_reader_);
		while (i > 0) {
			int type = xmlTextReaderNodeType(text_reader_);
			if (type == XML_READER_TYPE_ELEMENT) {
				const xmlChar *local_name = xmlTextReaderConstLocalName(text_reader_);
				if (xmlStrEqual(local_name, reinterpret_cast<const xmlChar *>("gaussian"))) {
					if (parameter->type() != Parameter::Type::kRandom) {
						std::cerr << "found <gaussian> for <parameter> of type ";
						parameter->PrintType(std::cerr);
						std::cerr << std::endl;
						return -2;
					}
					i = ReadGaussian(sb, parameter.get());
					if (i <= 0)
						return i;
					continue;
				} else if (xmlStrEqual(local_name, reinterpret_cast<const xmlChar *>("range"))) {
					if (parameter->type() != Parameter::Type::kSequence) {
						std::cerr << "found <range> for <parameter> of type ";
						parameter->PrintType(std::cerr);
						std::cerr << std::endl;
						return -2;
					}
					i = ReadRange(sb, parameter.get());
					if (i <= 0) return i;
					continue;
				} else {
					std::cerr << "unknown child of <parameter>: " << local_name << std::endl;
					return -2;
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

	int ReadGaussian(SampleBuilder *sb, Parameter *parameter) {
		bool count_found = false, mean_found = false, stddev_found = false;
		int count = 0;
		double mean, stddev;
		int i;
		while ( (i = xmlTextReaderMoveToNextAttribute(text_reader_)) > 0) {
			if (xmlTextReaderIsNamespaceDecl(text_reader_))
				continue;
			const xmlChar *local_name = xmlTextReaderConstLocalName(text_reader_);
			if (xmlStrEqual(local_name, reinterpret_cast<const xmlChar *>("count"))) {
				if (count_found) {
					std::cerr << "duplicate count of <gaussian>" << std::endl;
					return -2;
				}
				count_found = true;
				count = std::atoi(reinterpret_cast<const char *>(xmlTextReaderConstValue(text_reader_)));
			} else if (xmlStrEqual(local_name, reinterpret_cast<const xmlChar *>("mean"))) {
				if (mean_found) {
					std::cerr << "duplicate mean of <gaussian>" << std::endl;
					return -2;
				}
				mean_found = true;
				mean = std::atof(reinterpret_cast<const char *>(xmlTextReaderConstValue(text_reader_)));
			} else if (xmlStrEqual(local_name, reinterpret_cast<const xmlChar *>("stddev"))) {
				if (stddev_found) {
					std::cerr << "duplicate stddev of <gaussian>" << std::endl;
					return -2;
				}
				stddev_found = true;
				stddev = std::atof(reinterpret_cast<const char *>(xmlTextReaderConstValue(text_reader_)));
			} else {
				std::cerr << "unknown attribute of <gaussian>: " << local_name << std::endl;
				return -2;
			}
		}
		if (!count_found) {
			std::cerr << "missing count of <gaussian>" << std::endl;
			return -2;
		}
		if (count < 0) {
			std::cerr << "invalid count of <gaussian>" << count << std::endl;
			return -2;
		}
		if (!mean_found) {
			std::cerr << "missing mean of <gaussian>" << std::endl;
			return -2;
		}
		if (!stddev_found) {
			std::cerr << "missing stddev of <gaussian>" << std::endl;
			return -2;
		}

		assert(sd_param_);
		int e = sqlite3_bind_text(sd_param_->stmt(), 1, reinterpret_cast<const char *>(parameter->name()), -1, SQLITE_STATIC);
		if (e != SQLITE_OK) {
			std::cerr << "failed to bind parameter: " << e << std::endl;
			return -2;
		}

		std::normal_distribution<> nd(mean, stddev);
		std::vector<double> values;
		std::ostringstream oss;
		RequestMaxNumOfDigitsForDouble(oss);
		for (int i=0;i<count;i++) {
			double d = nd(gen_);
			if (i > 0)
				oss << ',';
			oss << d;
			values.push_back(d);
		}
		std::string s = oss.str();
		e = sqlite3_bind_text(sd_param_->stmt(), 2, s.c_str(), -1, nullptr);
		if (e != SQLITE_OK) {
			std::cerr << "failed to bind parameter: " << e << std::endl;
			return -2;
		}
		e = sqlite3_step(sd_param_->stmt());
		if (e != SQLITE_DONE) {
			std::cerr << "failed to step statement: " << e << std::endl;
			return -2;
		}
		sqlite3_reset(sd_param_->stmt());

		sb->PushParameter(reinterpret_cast<const char *>(parameter->name()), std::move(values));

		return xmlTextReaderRead(text_reader_);
	}

	int ReadRange(SampleBuilder *sb, Parameter *parameter) {
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
					std::cerr << "unknown type of <range>: " << type << std::endl;
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

		assert(sd_param_);
		int e = sqlite3_bind_text(sd_param_->stmt(), 1, reinterpret_cast<const char *>(parameter->name()), -1, SQLITE_STATIC);
		if (e != SQLITE_OK) {
			std::cerr << "failed to bind parameter: " << e << std::endl;
			return -2;
		}

		std::vector<double> values;
		if (range->type() == Range::kInterval) {
			std::ostringstream oss;
			RequestMaxNumOfDigitsForDouble(oss);
			boost::rational<long> d = range->lower();
			oss << boost::rational_cast<double>(d);
			values.push_back(boost::rational_cast<double>(d));
			for (;;) {
				d += range->step();
				if (d > range->upper()) break;
				oss << ',' << boost::rational_cast<double>(d);
				values.push_back(boost::rational_cast<double>(d));
			}
			std::string s(oss.str());
			size_t len = s.size();
			char *p = static_cast<char *>(malloc(len));
			if (!p) {
				std::cerr << "failed to malloc: " << len << std::endl;
				return -2;
			}
			memcpy(p, s.c_str(), len);
			e = sqlite3_bind_text(sd_param_->stmt(), 2, p, len, free);
			if (e != SQLITE_OK) {
				std::cerr << "failed to bind parameter: " << e << std::endl;
				return -2;
			}
		} else { // enum
			i = xmlTextReaderMoveToElement(text_reader_);
			if (i < 0) return i;
			xmlChar *enum_text = xmlTextReaderReadString(text_reader_);
			if (!ParseEnum(reinterpret_cast<char *>(enum_text), &values)) {
				xmlFree(enum_text);
				return false;
			}
			e = sqlite3_bind_text(sd_param_->stmt(), 2, reinterpret_cast<char *>(enum_text), -1, xmlFree);
			if (e != SQLITE_OK) {
				std::cerr << "failed to bind parameter: " << e << std::endl;
				return -2;
			}
		}
		e = sqlite3_step(sd_param_->stmt());
		if (e != SQLITE_DONE) {
			std::cerr << "failed to step statement: " << e << std::endl;
			return -2;
		}
		sqlite3_reset(sd_param_->stmt());

		sb->PushParameter(reinterpret_cast<const char *>(parameter->name()), std::move(values));

		return xmlTextReaderRead(text_reader_);
	}

	int ReadProduct(SampleBuilder *sb) {
		sb->PushProduct();
		int i = xmlTextReaderRead(text_reader_);
		while (i > 0) {
			int type = xmlTextReaderNodeType(text_reader_);
			if (type == XML_READER_TYPE_ELEMENT) {
				const xmlChar *local_name = xmlTextReaderConstLocalName(text_reader_);
				if (xmlStrEqual(local_name, reinterpret_cast<const xmlChar *>("parameter"))) {
					i = ReadParameter(sb);
					if (i <= 0)
						return i;
					continue;
				} if (xmlStrEqual(local_name, reinterpret_cast<const xmlChar *>("product"))) {
					i = ReadProduct(sb);
					if (i <= 0)
						return i;
					continue;
				} if (xmlStrEqual(local_name, reinterpret_cast<const xmlChar *>("zip"))) {
					i = ReadZip(sb);
					if (i <= 0)
						return i;
					continue;
				} else {
					std::cerr << "unknown child of <product>: " << local_name << std::endl;
					return -2;
				}
			} else if (type == XML_READER_TYPE_END_ELEMENT) {
				const xmlChar *local_name = xmlTextReaderConstLocalName(text_reader_);
				if (xmlStrEqual(local_name, reinterpret_cast<const xmlChar *>("product"))) {
					sb->Pop();
					return 1;
				}
			}
			i = xmlTextReaderRead(text_reader_);
		}
		return i;
	}

	int ReadZip(SampleBuilder *sb) {
		sb->PushZip();
		int i = xmlTextReaderRead(text_reader_);
		while (i > 0) {
			int type = xmlTextReaderNodeType(text_reader_);
			if (type == XML_READER_TYPE_ELEMENT) {
				const xmlChar *local_name = xmlTextReaderConstLocalName(text_reader_);
				if (xmlStrEqual(local_name, reinterpret_cast<const xmlChar *>("parameter"))) {
					i = ReadParameter(sb);
					if (i <= 0)
						return i;
					continue;
				} if (xmlStrEqual(local_name, reinterpret_cast<const xmlChar *>("product"))) {
					i = ReadProduct(sb);
					if (i <= 0)
						return i;
					continue;
				} if (xmlStrEqual(local_name, reinterpret_cast<const xmlChar *>("zip"))) {
					i = ReadZip(sb);
					if (i <= 0)
						return i;
					continue;
				} else {
					std::cerr << "unknown child of <zip>: " << local_name << std::endl;
					return -2;
				}
			} else if (type == XML_READER_TYPE_END_ELEMENT) {
				const xmlChar *local_name = xmlTextReaderConstLocalName(text_reader_);
				if (xmlStrEqual(local_name, reinterpret_cast<const xmlChar *>("zip"))) {
					sb->Pop();
					return 1;
				}
			}
			i = xmlTextReaderRead(text_reader_);
		}
		return i;
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
						int n_bytes = std::sprintf(query, "INSERT INTO db%d.phsp_targets values (?, ?, ?)", rowid);
						assert(n_bytes > 0);
						sqlite3_stmt *stmt;
						int e = sqlite3_prepare_v2(db_, query, n_bytes+1, &stmt, nullptr);
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
	std::unique_ptr<db::StatementDriver> sd_param_;
	std::random_device rd_;
	std::mt19937 gen_;
};

}

bool Read(const char *phsp_file, sqlite3 *db)
{
	boost::filesystem::path pp = GetPathFromUtf8(phsp_file);
	if (pp.empty())
		return false;
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
