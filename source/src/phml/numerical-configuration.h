/* -*- Mode: C++; tab-width: 4; indent-tabs-mode: t; c-basic-offset: 4 -*- vim:set ts=4 sw=4 sts=4 noet: */
#ifndef FLINT_PHML_NUMERICAL_CONFIGURATION_H_
#define FLINT_PHML_NUMERICAL_CONFIGURATION_H_

#include <libxml/globals.h>
#include <libxml/xmlstring.h>

namespace flint {
namespace phml {

class NumericalConfiguration {
public:
	NumericalConfiguration(const NumericalConfiguration &) = delete;
	NumericalConfiguration &operator=(const NumericalConfiguration &) = delete;

	NumericalConfiguration()
		: rg_name_(nullptr),
		  rg_seed_(nullptr),
		  integration_(nullptr),
		  sts_unit_id_(),
		  sts_value_(nullptr)
	{}

	~NumericalConfiguration() {
		if (rg_name_) xmlFree(rg_name_);
		if (rg_seed_) xmlFree(rg_seed_);
		if (integration_) xmlFree(integration_);
		if (sts_value_) xmlFree(sts_value_);
	}

	const xmlChar *rg_name() const {return rg_name_;}
	void set_rg_name(xmlChar *rg_name) {rg_name_ = rg_name;}
	const xmlChar *rg_seed() const {return rg_seed_;}
	void set_rg_seed(xmlChar *rg_seed) {rg_seed_ = rg_seed;}
	const xmlChar *integration() const {return integration_;}
	void set_integration(xmlChar *integration) {integration_ = integration;}
	int sts_unit_id() const {return sts_unit_id_;}
	void set_sts_unit_id(int sts_unit_id) {sts_unit_id_ = sts_unit_id;}
	const xmlChar *sts_value() const {return sts_value_;}
	void set_sts_value(xmlChar *sts_value) {sts_value_ = sts_value;}

private:
	xmlChar *rg_name_;
	xmlChar *rg_seed_;
	xmlChar *integration_;
	int sts_unit_id_;
	xmlChar *sts_value_;
};

}
}

#endif
