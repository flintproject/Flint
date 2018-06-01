/* -*- Mode: C++; tab-width: 4; indent-tabs-mode: t; c-basic-offset: 4 -*- vim:set ts=4 sw=4 sts=4 noet: */
#ifndef FLINT_CTRL_H_
#define FLINT_CTRL_H_

#include <atomic>
#include <condition_variable>
#include <mutex>

namespace flint {
namespace ctrl {

struct Argument {
	std::atomic<bool> paused;
	std::mutex mutex;
	std::condition_variable cv;
};

}
}

#endif
