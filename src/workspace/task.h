/* -*- Mode: C++; tab-width: 4; indent-tabs-mode: t; c-basic-offset: 4 -*- vim:set ts=4 sw=4 sts=4 noet: */
#ifndef FLINT_WORKSPACE_TASK_H_
#define FLINT_WORKSPACE_TASK_H_

#include <string>
#include "database.h"

namespace workspace {

class Task {
public:
	/*
	 * `model_file' should be a UTF-8 filename.
	 */
	explicit Task(const char *model_file, int task_id = 0)
		: model_file_(model_file),
		  task_id_(task_id) {
	}

	bool Setup() {
		char db_file[32]; // FIXME
		if (task_id_) {
			std::sprintf(db_file, "%d/model", task_id_);
		} else {
			std::sprintf(db_file, "model");
		}
		return SaveModelFile(db_file, model_file_) > 0;
	}

private:
	const char *model_file_;
	int task_id_;
};

} // namespace workspace

#endif
