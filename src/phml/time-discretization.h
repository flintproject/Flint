/* -*- Mode: C++; tab-width: 4; indent-tabs-mode: t; c-basic-offset: 4 -*- vim:set ts=4 sw=4 sts=4 noet: */
#ifndef FLINT_PHML_TIME_DISCRETIZATION_H_
#define FLINT_PHML_TIME_DISCRETIZATION_H_

#include <libxml/globals.h>
#include <libxml/xmlstring.h>

namespace flint {
namespace phml {

class TimeDiscretization {
public:
	TimeDiscretization(const TimeDiscretization &) = delete;
	TimeDiscretization &operator=(const TimeDiscretization &) = delete;

	TimeDiscretization()
		: unit_id_(),
		  step_(NULL),
		  module_id_(NULL)
	{
	}

	~TimeDiscretization() {
		if (step_) xmlFree(step_);
		if (module_id_) xmlFree(module_id_);
	}

	int unit_id() const {return unit_id_;}
	void set_unit_id(int unit_id) {unit_id_ = unit_id;}
	const xmlChar *step() const {return step_;}
	void set_step(xmlChar *step) {step_ = step;}

	const xmlChar *module_id() const {return module_id_;}
	void set_module_id(xmlChar *module_id) {module_id_ = module_id;}

private:
	int unit_id_;
	xmlChar *step_;
	xmlChar *module_id_;
};

}
}

#endif
