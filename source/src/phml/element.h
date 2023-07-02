/* -*- Mode: C++; tab-width: 4; indent-tabs-mode: t; c-basic-offset: 4 -*- vim:set ts=4 sw=4 sts=4 noet: */
#ifndef FLINT_PHML_ELEMENT_H_
#define FLINT_PHML_ELEMENT_H_

namespace flint {
namespace phml {

class Element {
public:
	Element(const Element &) = delete;
	Element &operator=(const Element &) = delete;

	Element()
		: unit_id_(-1) // initialized with an invalid value for error detection
		, exponent_()
		, factor_()
		, multiplier_()
		, offset_()
	{}

	int unit_id() const {return unit_id_;}
	void set_unit_id(int unit_id) {unit_id_ = unit_id;}
	double exponent() const {return exponent_;}
	void set_exponent(double exponent) {exponent_ = exponent;}
	int factor() const {return factor_;}
	void set_factor(int factor) {factor_ = factor;}
	double multiplier() const {return multiplier_;}
	void set_multiplier(double multiplier) {multiplier_ = multiplier;}
	double offset() const {return offset_;}
	void set_offset(double offset) {offset_ = offset;}

private:
	int unit_id_;
	double exponent_;
	int factor_;
	double multiplier_;
	double offset_;
};

}
}

#endif
