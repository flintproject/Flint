/* -*- Mode: C++; tab-width: 4; indent-tabs-mode: t; c-basic-offset: 4 -*- vim:set ts=4 sw=4 sts=4 noet: */
#ifndef FLINT_COMPILER_BCC_TOKENIZER_H_
#define FLINT_COMPILER_BCC_TOKENIZER_H_

#include <memory>
#include <ostream>

namespace flint {
namespace compiler {
namespace bcc {

struct Token;
class TokenizerImpl;

class Tokenizer {
public:
	explicit Tokenizer(const char *input);

	~Tokenizer();

	int operator()(Token *token);

	std::ostream &Dump(std::ostream &os) const;

private:
	std::unique_ptr<TokenizerImpl> impl_;
};

}
}
}

#endif
