/* -*- Mode: C++; tab-width: 4; indent-tabs-mode: t; c-basic-offset: 4 -*- vim:set ts=4 sw=4 sts=4 noet: */
#ifndef FLINT_TASK_CONFIG_READER_HH_
#define FLINT_TASK_CONFIG_READER_HH_

#include "compiler.hh"
#include "db/statement-driver.hh"

#include <memory>

namespace flint {
namespace task {

class ConfigReader : db::StatementDriver {
public:
	explicit ConfigReader(sqlite3 *db);

	~ConfigReader();

	double length() const {return length_;}
	double step() const {return step_;}
	int granularity() const {return granularity_;}
	double output_start_time() const {return output_start_time_;}

	bool Read();

	compiler::Method GetMethod() const;

private:
	/*
	 * possible values:
	 * "4th-rungekutta" from PHML
	 * "euler"
	 * "rk4"
	 */
	std::unique_ptr<char[]> method_;
	double length_;
	double step_;
	int granularity_;
	double output_start_time_;
};

}
}

#endif
