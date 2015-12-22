/* -*- Mode: C++; tab-width: 4; indent-tabs-mode: t; c-basic-offset: 4 -*- vim:set ts=4 sw=4 sts=4 noet: */
#ifndef FLINT_PHML_ARC_H_
#define FLINT_PHML_ARC_H_

#include <sstream>

namespace flint {
namespace phml {

class Arc {
public:
	enum Type {
		kUnspecified,
		kCondition,
		kProbability
	};

	Arc(int arc_id, int tail_node_id, int head_node_id)
		: arc_id_(arc_id)
		, tail_node_id_(tail_node_id)
		, head_node_id_(head_node_id)
		, type_(kUnspecified)
	{}

	int arc_id() const {return arc_id_;}
	int tail_node_id() const {return tail_node_id_;}
	int head_node_id() const {return head_node_id_;}
	Type type() const {return type_;}
	void set_type(Type type) {type_ = type;}

	std::ostringstream &stream() {return stream_;}

	std::string GetMath() const {
		return stream_.str();
	}

private:
	int arc_id_;
	int tail_node_id_;
	int head_node_id_;
	Type type_;
	std::ostringstream stream_;
};

}
}

#endif
