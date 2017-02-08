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

#include <boost/uuid/uuid_io.hpp>
#include <czmq.h>
#include <zmq.h>

namespace flint {
namespace fppp {

Publisher::Publisher(void *ctx, const char *hostname)
	: sock_(zmq_socket(ctx, ZMQ_PUB))
{
	assert(sock_);
	std::sprintf(endpoint_, "tcp://%s:*", hostname);
	int r = zmq_bind(sock_, endpoint_);
	if (r == 0) {
		size_t len;
		zmq_getsockopt(sock_, ZMQ_LAST_ENDPOINT, &endpoint_, &len);
		std::cerr << "endpoint: " << endpoint_ << std::endl;
	} else {
		std::cerr << "failed to bind socket: " << zmq_strerror(errno) << std::endl;
	}
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


Subscriber::Subscriber(void *ctx, const char *endpoint)
	: sock_(zmq_socket(ctx, ZMQ_SUB))
{
	assert(sock_);
	int r = zmq_setsockopt(sock_, ZMQ_SUBSCRIBE, "", 0);
	assert(r == 0);
	r = zmq_connect(sock_, endpoint);
	if (r != 0)
		std::cerr << "failed to connect socket: " << zmq_strerror(errno) << std::endl;
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

bool ShakeHands(void *ctx,
				std::vector<KeyData> kdv,
				Publisher **pub,
				Subscriber **sub)
{
	assert(ctx);

	zactor_t *beacon = zactor_new(zbeacon, nullptr);
	assert(beacon);
	//zstr_send(beacon, "VERBOSE");
	zsock_send(beacon, "si", "CONFIGURE", 5670);
	char *hostname = zstr_recv(beacon);
	if (!*hostname) {
		std::cerr << "failed to configure beacon" << std::endl;
		zstr_free(&hostname);
		zactor_destroy(&beacon);
		return false;
	}

	std::unique_ptr<Publisher> p(new Publisher(ctx, hostname));

	int n = std::rand();
	char peer_name[16];
	std::sprintf(peer_name, "fppp%d", n);
	std::cerr << "peer_name: " << peer_name << std::endl;
	zactor_t *peer = zactor_new(zgossip, peer_name);
	assert(peer);
	//zstr_send(peer, "VERBOSE");
	char endpoint[64];
	std::sprintf(endpoint, "tcp://%s:*", hostname);
	zstr_free(&hostname);
	zstr_sendx(peer, "BIND", endpoint, nullptr);
	zstr_sendx(peer, "PORT", nullptr);
	char *command, *port_str;
	zstr_recvx(peer, &command, &port_str, nullptr);
	assert(std::strcmp(command, "PORT") == 0);
	zstr_free(&command);

	zsock_send(beacon, "sbi", "PUBLISH", port_str, std::strlen(port_str), 250);
	zsock_send(beacon, "sb", "SUBSCRIBE", "", 0);

	char *received;
	// Poll on three API sockets at once
	zpoller_t *poller = zpoller_new(beacon, nullptr);
	assert(poller);
	std::int64_t stop_at = zclock_mono() + 100000;
	while (zclock_mono() < stop_at) {
		long timeout = static_cast<long>(stop_at - zclock_mono());
		if (timeout < 0)
			timeout = 0;
		void *which = zpoller_wait(poller, timeout * ZMQ_POLL_MSEC);
		if (which) {
			assert(which == beacon);
			char *ipaddress;
			zstr_recvx(beacon, &ipaddress, &received, nullptr);
			std::cerr << "ipaddress: " << ipaddress << std::endl;
			std::cerr << "received: " << received << std::endl;

			std::sprintf(endpoint, "tcp://%s:%s", ipaddress, received);
			zstr_free(&ipaddress);
			zstr_free(&received);
			break;
		}
	}
	zpoller_destroy(&poller);

	zactor_t *speaker = zactor_new(zgossip, received);
	assert(speaker);
	//zstr_send(speaker, "VERBOSE");
	zstr_sendx(speaker, "CONNECT", endpoint, nullptr);

	std::cerr << "count: " << kdv.size() << std::endl;
	for (const auto &kd : kdv) {
		char key[256];
		std::sprintf(key, "%s:%s", boost::uuids::to_string(kd.uuid).c_str(), kd.name.c_str());
		zstr_sendx(speaker, "PUBLISH", key, p->endpoint(), nullptr);
	}

	zclock_sleep(1000);
	zstr_sendx(beacon, "SILENCE", nullptr); // stop broadcasting
	zactor_destroy(&beacon);

	zstr_sendx(peer, "STATUS", nullptr);
	std::unordered_set<std::string> endpoints;
	for (;;) {
		char *x, *y;
		zstr_recvx(peer, &command, &x, &y, nullptr);
		if (std::strcmp(command, "STATUS") == 0) {
			std::cerr << "status: " << x << std::endl;
			break;
		} else if (std::strcmp(command, "DELIVER") == 0) {
			std::cerr << "deliver: " << y << std::endl;
			endpoints.emplace(y);
		}
		zstr_free(&command);
		zstr_free(&x);
		zstr_free(&y);
	}

	zactor_destroy(&speaker);
	zactor_destroy(&peer);

	*pub = p.release();
	if (sub) {
		if (endpoints.empty())
			*sub = nullptr;
		else
			*sub = new Subscriber(ctx, endpoints.begin()->c_str());
	}
	return true;
}

}
}
