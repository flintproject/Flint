/* -*- Mode: C++; tab-width: 4; indent-tabs-mode: t; c-basic-offset: 4 -*- vim:set ts=4 sw=4 sts=4 noet: */
#include "gui/preference.h"

#include <thread>

namespace flint {
namespace gui {

Preference::Preference()
	: concurrency(static_cast<int>(std::thread::hardware_concurrency()))
{
}

Preference::~Preference() = default;

}
}
