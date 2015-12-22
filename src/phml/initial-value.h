/* -*- Mode: C++; tab-width: 4; indent-tabs-mode: t; c-basic-offset: 4 -*- vim:set ts=4 sw=4 sts=4 noet: */
#ifndef FLINT_PHML_INITIAL_VALUE_H_
#define FLINT_PHML_INITIAL_VALUE_H_

#include <sstream>
#include <string>

namespace flint {
namespace phml {

class InitialValue {
public:
	static const char *kName;

	InitialValue(const InitialValue &) = delete;
	InitialValue &operator=(const InitialValue &) = delete;

	InitialValue() {}

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
	std::ostringstream stream_;
};

}
}

#endif
