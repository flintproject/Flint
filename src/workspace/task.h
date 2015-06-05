/* -*- Mode: C++; tab-width: 4; indent-tabs-mode: t; c-basic-offset: 4 -*- vim:set ts=4 sw=4 sts=4 noet: */
#ifndef FLINT_WORKSPACE_TASK_H_
#define FLINT_WORKSPACE_TASK_H_

#include "file.hh"

namespace workspace {

class Task {
public:
	/*
	 * `given_file' should be a UTF-8 filename.
	 */
	explicit Task(const char *given_file, int task_id = 0);

	bool Setup(file::Format *format);

private:
	const char *given_file_;
	int task_id_;
};

} // namespace workspace

#endif
