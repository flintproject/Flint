/* -*- Mode: C++; tab-width: 4; indent-tabs-mode: t; c-basic-offset: 4 -*- vim:set ts=4 sw=4 sts=4 noet: */
#ifndef FLINT_FPPP_UTILITY_H_
#define FLINT_FPPP_UTILITY_H_

#include <cassert>

#include <zmq.h>

namespace flint {
namespace fppp {

class ContextGuard {
public:
	explicit ContextGuard(void *ctx)
		: ctx_(ctx)
	{
		assert(ctx);
	}

	~ContextGuard() {
		zmq_ctx_shutdown(ctx_);
		zmq_ctx_term(ctx_);
	}

private:
	void *ctx_;
};


}
}

#endif
