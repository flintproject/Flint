/* -*- Mode: C++; tab-width: 4; indent-tabs-mode: t; c-basic-offset: 4 -*- vim:set ts=4 sw=4 sts=4 noet: */
#ifdef HAVE_CONFIG_H
#include "config.h"
#endif

#include "task.h"

namespace flint {
namespace task {

bool Task::IsCanceled() const
{
	return control_mr.get_size() > 0 &&
		control_mr.get_address() &&
		*static_cast<char *>(control_mr.get_address()) == '\1';
}

}
}
