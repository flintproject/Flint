/* -*- Mode: C++; tab-width: 4; indent-tabs-mode: t; c-basic-offset: 4 -*- vim:set ts=4 sw=4 sts=4 noet: */
#ifndef FLINT_PHML_EXTRA_IMPLEMENTATION_H_
#define FLINT_PHML_EXTRA_IMPLEMENTATION_H_

#include <memory>
#include <sstream>
#include <string>

#include <libxml/xmlstring.h>

namespace flint {
namespace phml {

class Definition;

class ExtraImplementation {
public:
	ExtraImplementation(const ExtraImplementation &) = delete;
	ExtraImplementation &operator=(const ExtraImplementation &) = delete;

	static const char *kName;

	ExtraImplementation();

	~ExtraImplementation();

	const xmlChar *type() const {return type_;}
	void set_type(xmlChar *type) {type_ = type;}
	const xmlChar *order() const {return order_;}
	void set_order(xmlChar *order) {order_ = order;}
	const Definition *definition() const;
	void set_definition(Definition *definition);

	void OpenDefinition(int /*level*/) const {}

	void CloseDefinition(int /*level*/) const {}

	std::ostream *GetOutputStream() {
		return &stream_;
	}

	void Print(const char *s) {
		stream_ << s;
	}

	std::string GetString() const {
		return stream_.str();
	}

	bool IsProper() const {
		return !stream_.str().empty();
	}

private:
	xmlChar *type_;
	xmlChar *order_;
	std::unique_ptr<Definition> definition_;
	std::ostringstream stream_;
};

}
}

#endif
