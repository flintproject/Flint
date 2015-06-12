/* -*- Mode: C++; tab-width: 4; indent-tabs-mode: t; c-basic-offset: 4 -*- vim:set ts=4 sw=4 sts=4 noet: */
#ifndef FLINT_TASK_CONFIG_READER_HH_
#define FLINT_TASK_CONFIG_READER_HH_

#include "db/statement-driver.h"

namespace task {

class ConfigReader : db::StatementDriver {
public:
	explicit ConfigReader(sqlite3 *db);

	~ConfigReader();

	const char *method() const {return method_;}
	double length() const {return length_;}
	double step() const {return step_;}
	int granularity() const {return granularity_;}

	bool Read();

private:
	char *method_;
	double length_;
	double step_;
	int granularity_;
};

}

#endif
