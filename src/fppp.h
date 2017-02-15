/* -*- Mode: C++; tab-width: 4; indent-tabs-mode: t; c-basic-offset: 4 -*- vim:set ts=4 sw=4 sts=4 noet: */
#ifndef FLINT_FPPP_H_
#define FLINT_FPPP_H_

#include <set>
#include <string>
#include <unordered_set>
#include <vector>

#include <boost/uuid/uuid.hpp>
#include <czmq.h>

namespace flint {
namespace fppp {

struct KeyData
{
	boost::uuids::uuid uuid;
	std::string name;

	bool operator<(const KeyData &other) const;
};

class Publisher {
public:
	Publisher(void *ctx, const char *hostname);
	~Publisher();

	char *endpoint() {return endpoint_;}

	void operator()(boost::uuids::uuid uuid, std::string name, const char *time, const char *value);

private:
	void *sock_;
	char endpoint_[64];
	char data_[64];
};

class Subscriber {
public:
	Subscriber(void *ctx,
			   const std::unordered_set<std::string> &endpoints,
			   const std::set<KeyData> &data);
	~Subscriber();

	void operator()(void (*f)(boost::uuids::uuid uuid, std::string name, const char *time, const char *value));

private:
	void *sock_;
};

zactor_t *ShakeHands(void *ctx,
					 const char *host,
					 std::set<KeyData> &in,
					 const std::vector<KeyData> &out,
					 Publisher **pub,
					 Subscriber **sub);

}
}

#endif
