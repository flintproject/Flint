/* -*- Mode: C++; tab-width: 4; indent-tabs-mode: t; c-basic-offset: 4 -*- vim:set ts=4 sw=4 sts=4 noet: */
#ifndef FLINT_FPPP_H_
#define FLINT_FPPP_H_

#include <string>
#include <vector>

#include <boost/uuid/uuid.hpp>

namespace flint {
namespace fppp {

struct KeyData
{
	boost::uuids::uuid uuid;
	std::string name;
};

class Publisher {
public:
	Publisher(void *ctx, const char *hostname);
	~Publisher();

	const char *endpoint() const {return endpoint_;}

	void operator()(boost::uuids::uuid uuid, std::string name, const char *time, const char *value);

private:
	void *sock_;
	char endpoint_[64];
	char data_[64];
};

class Subscriber {
public:
	Subscriber(void *ctx, const char *endpoint);
	~Subscriber();

	void operator()(void (*f)(boost::uuids::uuid uuid, std::string name, const char *time, const char *value));

private:
	void *sock_;
};

bool ShakeHands(void *ctx,
				std::vector<KeyData> kdv,
				Publisher **pub,
				Subscriber **sub = nullptr);

}
}

#endif
