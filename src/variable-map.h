/* -*- Mode: C++; tab-width: 4; indent-tabs-mode: t; c-basic-offset: 4 -*- vim:set ts=4 sw=4 sts=4 noet: */
#ifndef FLINT_VARIABLE_MAP_H_
#define FLINT_VARIABLE_MAP_H_

#include <string>
#include <unordered_map>

#include <boost/functional/hash.hpp>
#include <boost/uuid/uuid.hpp>

#include "sqlite3.h"

namespace flint {

class Variable;

class VariableMap {
public:
	void Add(const boost::uuids::uuid &u, std::unique_ptr<Variable> &&v);

	bool Load(sqlite3 *db);

	const Variable *Find(const boost::uuids::uuid &u,
						 const std::string &name) const;

private:
	typedef std::unordered_map<std::string,
							   std::unique_ptr<Variable>
							   > SubMap;

	std::unordered_map<boost::uuids::uuid,
					   SubMap,
					   boost::hash<boost::uuids::uuid>
					   > m_;
};

}

#endif
