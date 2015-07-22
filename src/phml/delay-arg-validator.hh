/* -*- Mode: C++; tab-width: 4; indent-tabs-mode: t; c-basic-offset: 4 -*- vim:set ts=4 sw=4 sts=4 noet: */
#ifndef FLINT_PHML_DELAY_ARG_VALIDATOR_H_
#define FLINT_PHML_DELAY_ARG_VALIDATOR_H_

#include "sqlite3.h"

namespace phml {

class DelayArgValidator {
public:
	explicit DelayArgValidator(sqlite3 *db);

	bool Validate();

private:
	sqlite3 *db_;
};

}

#endif
