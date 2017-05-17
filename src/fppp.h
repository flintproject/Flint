/* -*- Mode: C++; tab-width: 4; indent-tabs-mode: t; c-basic-offset: 4 -*- vim:set ts=4 sw=4 sts=4 noet: */
#ifndef FLINT_FPPP_H_
#define FLINT_FPPP_H_

#include <map>
#include <set>
#include <string>
#include <unordered_set>
#include <vector>

#include <boost/uuid/uuid.hpp>
#include <czmq.h>

#include "flint/key.h"

namespace flint {
namespace fppp {

struct Option {
	const char *host;
	std::map<key::Data, size_t> output;
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
			   const std::set<key::Data> &data);
	~Subscriber();

	void operator()(void (*f)(boost::uuids::uuid uuid, std::string name, const char *time, const char *value));

private:
	void *sock_;
};

bool IsValidUrl(const char *url);

boost::uuids::uuid GetUuidFromUrl(const char *url);

zactor_t *ShakeHands(void *ctx,
					 const char *host,
					 std::set<key::Data> &in,
					 const std::vector<key::Data> &out,
					 Publisher **pub,
					 Subscriber **sub);

}
}

#endif
