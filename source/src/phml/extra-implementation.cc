/* -*- Mode: C++; tab-width: 4; indent-tabs-mode: t; c-basic-offset: 4 -*- vim:set ts=4 sw=4 sts=4 noet: */
#include "phml/extra-implementation.h"

#include <libxml/globals.h>

#include "phml/definition.h"

namespace flint {
namespace phml {

ExtraImplementation::ExtraImplementation()
	: type_(nullptr)
	, order_(nullptr)
{
}

ExtraImplementation::~ExtraImplementation()
{
	if (type_) xmlFree(type_);
	if (order_) xmlFree(order_);
}

const Definition *ExtraImplementation::definition() const
{
	return definition_.get();
}

void ExtraImplementation::set_definition(Definition *definition)
{
	definition_.reset(definition);
}

const char *ExtraImplementation::kName = "extra-implementation";

}
}
