/* -*- Mode: C++; tab-width: 4; indent-tabs-mode: t; c-basic-offset: 4 -*- vim:set ts=4 sw=4 sts=4 noet: */
#include "name.h"

#include <cassert>

namespace flint {

Name::Name(char type, int id, const char *name, const char *unit, double capacity)
	: type_(type)
	, id_(id)
	, name_(name)
	, unit_(unit)
	, capacity_(capacity)
{
	assert(unit);
}

}
