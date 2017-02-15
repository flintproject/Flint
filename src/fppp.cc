/* -*- Mode: C++; tab-width: 4; indent-tabs-mode: t; c-basic-offset: 4 -*- vim:set ts=4 sw=4 sts=4 noet: */
#ifdef HAVE_CONFIG_H
#include "config.h"
#endif

#include "fppp.h"

#include <cassert>
#include <cerrno>
#include <cstdio>
#include <cstdlib>
#include <cstring>
#include <iostream>
#include <memory>
#include <unordered_set>

#include <boost/uuid/string_generator.hpp>
#include <boost/uuid/uuid_io.hpp>
#include <czmq.h>
#include <zmq.h>

namespace flint {
namespace fppp {

bool KeyData::operator<(const KeyData &other) const
{
	if (uuid < other.uuid)
		return true;
	if (uuid == other.uuid)
		return name < other.name;
	return false;
}

Publisher::Publisher(void *ctx, const char *hostname)
	: sock_(zmq_socket(ctx, ZMQ_PUB))
{
	assert(sock_);
	std::sprintf(endpoint_, "tcp://%s:*", hostname);
	int r = zmq_bind(sock_, endpoint_);
	if (r != 0) {
		std::cerr << "failed to bind socket: "
				  << zmq_strerror(errno)
				  << std::endl;
		return;
	}
	size_t len = 64;
	r = zmq_getsockopt(sock_, ZMQ_LAST_ENDPOINT, &endpoint_, &len);
	if (r != 0) {
		std::cerr << "failed to get last endpoint: "
				  << zmq_strerror(errno)
				  << std::endl;
		return;
	}
	std::cerr << "endpoint: " << endpoint_ << std::endl;
}

Publisher::~Publisher()
{
	if (sock_)
		zmq_close(sock_);
}

void Publisher::operator()(boost::uuids::uuid uuid, std::string name, const char *time, const char *value)
{
	std::memset(data_, 0, sizeof(data_));
	std::memcpy(data_, &uuid, 16);
	std::memcpy(data_+16, name.c_str(), std::min(static_cast<size_t>(32), name.size()));
	std::memcpy(data_+48, time, 8);
	std::memcpy(data_+56, value, 8);
	zmq_send(sock_, data_, sizeof(data_), 0);
}


Subscriber::Subscriber(void *ctx,
					   const std::unordered_set<std::string> &endpoints,
					   const std::set<KeyData> &v)
	: sock_(zmq_socket(ctx, ZMQ_SUB))
{
	const size_t kPrefixSize = 48;
	assert(sock_);
	char prefix[kPrefixSize];
	for (auto &kd : v) {
		std::memset(prefix, 0, sizeof(prefix));
		std::memcpy(prefix, &kd.uuid, 16);
		std::memcpy(prefix+16, kd.name.c_str(), kd.name.size());
		int r = zmq_setsockopt(sock_, ZMQ_SUBSCRIBE, prefix, kPrefixSize);
		assert(r == 0);
	}
	for (auto &ep : endpoints) {
		int r = zmq_connect(sock_, ep.c_str());
		if (r != 0) {
			std::cerr << "failed to connect socket: "
					  << zmq_strerror(errno)
					  << std::endl;
			break;
		}
	}
}


Subscriber::~Subscriber()
{
	if (sock_)
		zmq_close(sock_);
}

void Subscriber::operator()(void (*f)(boost::uuids::uuid uuid, std::string name, const char *time, const char *value))
{
	char buf[64];
	boost::uuids::uuid uuid;
	for (;;) {
		int nbytes = zmq_recv(sock_, buf, sizeof(buf), 0);
		if (nbytes < 0) {
			std::cerr << "zmq_recv failed: " << zmq_strerror(errno) << std::endl;
			break;
		}
		std::memcpy(&uuid, buf, 16);
		f(uuid, buf+16, buf+48, buf+56);
	}
}

zactor_t *ShakeHands(void *ctx,
					 const char *host,
					 std::set<KeyData> &in,
					 const std::vector<KeyData> &out,
					 Publisher **pub,
					 Subscriber **sub)
{
	const size_t kLength = 128;

	assert(ctx);

	// get available address
	char address[64];
	auto *iflist = ziflist_new();
	assert(iflist);
	const char *name = ziflist_first(iflist);
	if (!name) {
		std::cerr << "failed to get interface name" << std::endl;
		ziflist_destroy(&iflist);
		return nullptr;
	}
	std::sprintf(address, "%s", ziflist_address(iflist));
	ziflist_destroy(&iflist);

	std::unique_ptr<Publisher> p(new Publisher(ctx, address));

	// register output
	zactor_t *peer = zactor_new(zgossip, p->endpoint());
	assert(peer);
	char endpoint[kLength];
	std::sprintf(endpoint, "tcp://%s:20010", host);
	zstr_sendx(peer, "CONNECT", endpoint, nullptr);
	std::cerr << "the number of out: " << out.size() << std::endl;
	for (const auto &kd : out) {
		char key[256];
		std::sprintf(key, "%s:%s", boost::uuids::to_string(kd.uuid).c_str(), kd.name.c_str());
		zstr_sendx(peer, "PUBLISH", key, p->endpoint(), nullptr);
	}

	std::unordered_set<std::string> endpoints;
	std::set<KeyData> data;
	zpoller_t *poller = zpoller_new(peer, nullptr);
	assert(poller);
	boost::uuids::string_generator gen;
	// poll until all of input become registered
	while (!in.empty()) {
		void *which = zpoller_wait(poller, -1); // no timeout
		if (!which) {
			zpoller_destroy(&poller);
			zactor_destroy(&peer);
			return nullptr;
		}
		assert(which == peer);
		char *command, *key, *value;
		zstr_recvx(peer, &command, &key, &value, nullptr);
		if (std::strcmp(command, "DELIVER") == 0) {
			KeyData kd;
			kd.uuid = gen(std::string(key, 36));
			kd.name = std::string(key+37);
			auto it = in.find(kd);
			if (it != in.end()) {
				data.insert(kd);
				endpoints.emplace(value);
				in.erase(it);
			}
		}
		zstr_free(&command);
		zstr_free(&key);
		zstr_free(&value);
	}
	zpoller_destroy(&poller);

	*pub = p.release();
	*sub = (endpoints.empty()) ? nullptr : new Subscriber(ctx, endpoints, data);
	return peer;
}

}
}
