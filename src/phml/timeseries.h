/* -*- Mode: C++; tab-width: 4; indent-tabs-mode: t; c-basic-offset: 4 -*- vim:set ts=4 sw=4 sts=4 noet: */
#ifndef FLINT_PHML_TIMESERIES_H_
#define FLINT_PHML_TIMESERIES_H_

#include <libxml/globals.h>
#include <libxml/xmlstring.h>

namespace flint {
namespace phml {

class Timeseries {
public:
	Timeseries(const Timeseries &) = delete;
	Timeseries &operator=(const Timeseries &) = delete;

	Timeseries()
		: timeseries_id_(nullptr)
		, format_(nullptr)
		, iref_(nullptr)
		, zref_(nullptr)
	{}

	~Timeseries() {
		if (timeseries_id_) xmlFree(timeseries_id_);
		if (format_) xmlFree(format_);
		if (iref_) xmlFree(iref_);
		if (zref_) xmlFree(zref_);
	}

	const xmlChar *timeseries_id() const {return timeseries_id_;}
	void set_timeseries_id(xmlChar *timeseries_id) {timeseries_id_ = timeseries_id;}
	const xmlChar *format() const {return format_;}
	void set_format(xmlChar *format) {format_ = format;}
	const xmlChar *iref() const {return iref_;}
	void set_iref(xmlChar *iref) {iref_ = iref;}
	const xmlChar *zref() const {return zref_;}
	void set_zref(xmlChar *zref) {zref_ = zref;}

private:
	xmlChar *timeseries_id_;
	xmlChar *format_;
	xmlChar *iref_;
	xmlChar *zref_;
};

}
}

#endif
