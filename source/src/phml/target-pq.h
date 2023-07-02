/* -*- Mode: C++; tab-width: 4; indent-tabs-mode: t; c-basic-offset: 4 -*- vim:set ts=4 sw=4 sts=4 noet: */
#ifndef FLINT_PHML_TARGET_PQ_H_
#define FLINT_PHML_TARGET_PQ_H_

#include <sstream>
#include <string>

namespace flint {
namespace phml {

class TargetPq {
public:
	static const char *kName;

	TargetPq(const TargetPq &) = delete;
	TargetPq &operator=(const TargetPq &) = delete;

	explicit TargetPq(int pq_id)
		: pq_id_(pq_id)
		, stream_()
	{}

	int pq_id() const {return pq_id_;}

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
	int pq_id_;
	std::ostringstream stream_;
};

}
}

#endif
