/* -*- Mode: C++; tab-width: 4; indent-tabs-mode: t; c-basic-offset: 4 -*- vim:set ts=4 sw=4 sts=4 noet: */
#ifndef FLINT_PHML_TRANSITION_FORM_H_
#define FLINT_PHML_TRANSITION_FORM_H_

#include "sqlite3.h"

namespace flint {
namespace phml {

class TransitionForm {
public:
	TransitionForm();

	~TransitionForm();

	bool operator()(sqlite3 *db);

private:
	sqlite3_stmt *stmt_select_;
	sqlite3_stmt *stmt_extras_;
	sqlite3_stmt *stmt_impls_;
};

}
}

#endif
