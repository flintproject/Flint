/* -*- Mode: C++; tab-width: 4; indent-tabs-mode: t; c-basic-offset: 4 -*- vim:set ts=4 sw=4 sts=4 noet: */
#ifndef FLINT_COMPILER_BCC_PARSER_H_
#define FLINT_COMPILER_BCC_PARSER_H_

#include <memory>
#include <vector>

#include "bc.pb.h"

namespace flint {
namespace compiler {
namespace bcc {

struct Body {
	std::vector<bc::Code> code;
	std::vector<int> labels;
};

class ParserImpl;

class Parser {
public:
	explicit Parser(const char *input);

	~Parser();

	bool operator()(Body *body);

private:
	std::unique_ptr<ParserImpl> impl_;
};

}
}
}

#endif
