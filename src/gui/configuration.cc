/* -*- Mode: C++; tab-width: 4; indent-tabs-mode: t; c-basic-offset: 4 -*- vim:set ts=4 sw=4 sts=4 noet: */
#ifdef HAVE_CONFIG_H
#include "config.h"
#endif

#include "gui/configuration.h"

#include <cassert>
#include <cmath>
#include <cstring>
#include <regex>

#include <boost/uuid/uuid_io.hpp>

#include "gui/document.h"

namespace flint {
namespace gui {

const char *Configuration::GetKisaoId() const
{
	if (method == "ARK")
		return "9999999";
	if (method == "Euler")
		return "0000030";
	return "0000032";
}

namespace {

/*
 * Return the scalar value of time length in the initial step unit
 */
double Convert(double d, int i, const Document *doc)
{
	int k = doc->initial_config().step_unit;
	d *= doc->GetNumerator(i) * doc->GetDenominator(k);
	d /= doc->GetDenominator(i) * doc->GetNumerator(k);
	return d;
}

double Convert(const wxString &s, int i, const Document *doc)
{
	double d;
	auto b = s.ToCDouble(&d);
	assert(b);
	return Convert(d, i, doc);
}

}

double Configuration::GetOutputStartTime(const Document *doc) const
{
	return Convert(start, start_unit, doc);
}

double Configuration::GetOutputEndTime(const Document *doc) const
{
	return Convert(length, length_unit, doc);
}

int Configuration::GetNumberOfPoints(const Document *doc) const
{
	auto l = Convert(length, length_unit, doc);
	auto s = Convert(step, step_unit, doc);
	return std::floor(l/s);
}

namespace {

const std::string &GetTargetString(const lo::Column &column, int filter_column)
{
	return (filter_column == 0) ? column.name() : column.track_name();
}

std::string GetKey(const lo::Column &column)
{
	boost::uuids::uuid uuid;
	std::memcpy(&uuid, column.uuid().c_str(), 16);
	return boost::uuids::to_string(uuid) + ":" + column.name();
}

}

void Configuration::GetOutputVariables(const Document *doc,
									   std::vector<lo::Column> *v) const
{
	if (filter_pattern == "Regular expression") {
		try {
			std::regex r(filter_value);
			for (auto &column : doc->var()) {
				if (std::regex_search(GetTargetString(column, filter_column), r))
					v->push_back(column);
			}
		} catch (const std::regex_error &e) {
			// ignore columns if given regexp is invalid
		}
	} else if (filter_pattern == "Wildcard") {
		for (auto &column : doc->var()) {
			// TODO
			v->push_back(column);
		}
	} else { // Fixed string
		for (auto &column : doc->var()) {
			auto found = GetTargetString(column, filter_column).find(filter_value);
			if (found != std::string::npos)
				v->push_back(column);
		}
	}
}

void Configuration::GetOutputVariables(const Document *doc,
									   std::vector<std::string> *v) const
{
	std::vector<lo::Column> columns;
	GetOutputVariables(doc, &columns);
	for (auto &column : columns)
		v->push_back(GetKey(column));
}

std::vector<std::pair<lo::Column, std::string> > Configuration::GetTargets(const Document *doc) const
{
	std::vector<std::pair<lo::Column, std::string> > v;
	int i = 0;
	for (const auto &column : doc->param()) {
		auto it = parameters.find(i++);
		if (it != parameters.end())
			v.emplace_back(column, it->second);
	}
	return v;
}

}
}
