/* -*- Mode: C++; tab-width: 4; indent-tabs-mode: t; c-basic-offset: 4 -*- vim:set ts=4 sw=4 sts=4 noet: */
#ifndef FLINT_PHML_VALIDATOR_H_
#define FLINT_PHML_VALIDATOR_H_

#include "db/statement-driver.h"

namespace flint {
namespace phml {

class VariableDefinitionValidator {
public:
	explicit VariableDefinitionValidator(sqlite3 *db);

	~VariableDefinitionValidator();

	bool Validate();

private:
	db::StatementDriver sd_diff_;
};

}
}

#endif
