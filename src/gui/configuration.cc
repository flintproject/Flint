/* -*- Mode: C++; tab-width: 4; indent-tabs-mode: t; c-basic-offset: 4 -*- vim:set ts=4 sw=4 sts=4 noet: */
#ifdef HAVE_CONFIG_H
#include "config.h"
#endif

#include "gui/configuration.h"

#include <cassert>
#include <cmath>
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

double Convert(const wxString &s, int i, const Document *doc)
{
	double d;
	auto b = s.ToCDouble(&d);
	assert(b);
	d *= doc->GetNumerator(i);
	d /= doc->GetDenominator(i);
	return d;
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
	return (filter_column == 0) ? column.name() : column.name();
}

std::string GetKey(const lo::Column &column)
{
	boost::uuids::uuid uuid;
	std::memcpy(&uuid, column.uuid().c_str(), 16);
	return boost::uuids::to_string(uuid) + ":" + column.name();
}

}

void Configuration::GetOutputVariables(const Document *doc,
									   std::vector<std::string> *v) const
{
	if (filter_pattern == "Regular expression") {
		std::regex r(filter_value);
		for (auto &column : doc->var()) {
			if (std::regex_search(GetTargetString(column, filter_column), r))
				v->push_back(GetKey(column));
		}
	} else if (filter_pattern == "Wildcard") {
		for (auto &column : doc->var()) {
			// TODO
			v->push_back(GetKey(column));
		}
	} else { // Fixed string
		for (auto &column : doc->var()) {
			auto found = GetTargetString(column, filter_column).find(filter_value);
			if (found != std::string::npos)
				v->push_back(GetKey(column));
		}
	}
}

}
}
