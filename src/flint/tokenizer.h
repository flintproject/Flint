/* -*- Mode: C++; tab-width: 4; indent-tabs-mode: t; c-basic-offset: 4 -*- vim:set ts=4 sw=4 sts=4 noet: */
#ifndef FLINT_TOKENIZER_H_
#define FLINT_TOKENIZER_H_

#include <memory>

namespace flint {

struct Token;

namespace tokenizer {

class Impl;

class Tokenizer {
public:
	explicit Tokenizer(const char *input);

	~Tokenizer();

	int operator()(Token *token);

private:
	std::unique_ptr<Impl> impl_;
};

}
}

#endif
