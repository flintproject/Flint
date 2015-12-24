/* -*- Mode: C++; tab-width: 4; indent-tabs-mode: t; c-basic-offset: 4 -*- vim:set ts=4 sw=4 sts=4 noet: */
#ifndef FLINT_PHML_EVENT_CONDITION_H_
#define FLINT_PHML_EVENT_CONDITION_H_

#include <sstream>
#include <string>

namespace flint {
namespace phml {

class EventCondition {
public:
	EventCondition(const EventCondition &) = delete;
	EventCondition &operator=(const EventCondition &) = delete;

	EventCondition() = default;

	~EventCondition() = default;

	std::ostringstream &stream() {return stream_;}

	std::string GetMath() const {
		return stream_.str();
	}

	int Handle(int i) {
		return i;
	}

private:
	std::ostringstream stream_;
};

}
}

#endif
