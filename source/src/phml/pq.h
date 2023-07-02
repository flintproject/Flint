/* -*- Mode: C++; tab-width: 4; indent-tabs-mode: t; c-basic-offset: 4 -*- vim:set ts=4 sw=4 sts=4 noet: */
#ifndef FLINT_PHML_PQ_H_
#define FLINT_PHML_PQ_H_

#include <libxml/globals.h>
#include <libxml/xmlstring.h>

#include "sqlite3.h"

namespace flint {
namespace phml {

class PQ {
public:
	enum Type {
		kUnknown,
		kState,
		kVariableParameter,
		kStaticParameter,
		kTimeseries
	};

	PQ(const PQ &) = delete;
	PQ &operator=(const PQ &) = delete;

	PQ()
		: type_(kUnknown)
		, pq_id_()
		, unit_id_(nullptr)
		, name_(nullptr)
		, col_(0)
		, row_(0)
		, max_delay_(nullptr)
		, rowid_()
	{}

	~PQ() {
		if (unit_id_) xmlFree(unit_id_);
		if (name_) xmlFree(name_);
		if (max_delay_) xmlFree(max_delay_);
	}

	Type type() const {return type_;}
	void set_type(Type type) {type_ = type;}
	int pq_id() const {return pq_id_;}
	void set_pq_id(int pq_id) {pq_id_ = pq_id;}
	const xmlChar *unit_id() const {return unit_id_;}
	void set_unit_id(xmlChar *unit_id) {unit_id_ = unit_id;}
	const xmlChar *name() const {return name_;}
	void set_name(xmlChar *name) {name_ = name;}
	int col() const {return col_;}
	void set_col(int col) {col_ = col;}
	int row() const {return row_;}
	void set_row(int row) {row_ = row;}
	const xmlChar *max_delay() const {return max_delay_;}
	void set_max_delay(xmlChar *max_delay) {max_delay_ = max_delay;}
	sqlite3_int64 rowid() const {return rowid_;}
	void set_rowid(sqlite3_int64 rowid) {rowid_ = rowid;}

	bool IsValid() const {
		return type_ != kUnknown && pq_id_ > 0;
	}

	const char *GetType() const {
		switch (type_) {
		case kState:             return "x";
		case kVariableParameter: return "v";
		case kStaticParameter:   return "s";
		case kTimeseries:        return "t";
		default:
			assert(false);
			return nullptr;
		}
	}

	const char *GetTypeName() const {
		switch (type_) {
		case kState:             return "state";
		case kVariableParameter: return "variable-parameter";
		case kStaticParameter:   return "static-parameter";
		case kTimeseries:        return "timeseries";
		default:
			assert(false);
			return nullptr;
		}
	}

	bool IsSaved() const {
		return rowid_ != 0;
	}

private:
	Type type_;
	int pq_id_;
	xmlChar *unit_id_;
	xmlChar *name_;
	int col_;
	int row_;
	xmlChar *max_delay_;
	sqlite3_int64 rowid_;
};

}
}

#endif
