/* -*- Mode: C++; tab-width: 4; indent-tabs-mode: t; c-basic-offset: 4 -*- vim:set ts=4 sw=4 sts=4 noet: */
#include "runtime/channel.h"

#include <cassert>
#include <cerrno>
#include <cstdio>
#include <cstring>
#include <iostream>
#include <map>
#include <memory>
#include <set>
#include <unordered_set>
#include <vector>

#include <boost/uuid/uuid_io.hpp>

#include "bc/index.h"
#include "czmq.h"
#include "zmq.h"

namespace flint {
namespace runtime {

namespace {

const size_t kBufferSize = 64;
const size_t kPrefixSize = 48;

int Process(void *data, int argc, char **argv, char **names)
{
	(void)names;
	assert(argc == 2);
	auto *cv = static_cast<std::vector<std::string> *>(data);
	size_t len = std::strlen(argv[1]);
	assert(16+len <= kPrefixSize);
	char prefix[kPrefixSize];
	std::memset(prefix, 0, kPrefixSize);
	std::memcpy(prefix, argv[0], 16);
	std::memcpy(prefix+16, argv[1], len);
	cv->emplace_back(std::string(prefix, kPrefixSize));
	return 0;
}

}

class ChannelImpl {
public:
	explicit ChannelImpl(const std::vector<std::string> &cv);

	~ChannelImpl();

	bool Connect(const char *host, const std::map<key::Data, size_t> &output);

	bool Lookup(int index, double t, double *d);

	void Send(const double *data);

private:
	std::vector<void *> sv_;
	std::vector<std::map<double, double> > v_;
	std::map<std::string, int> m_;
	std::map<std::string, size_t> output_;
	void *ctx_;
	void *sock_;
	char buf_[kBufferSize];
};

ChannelImpl::ChannelImpl(const std::vector<std::string> &v)
	: sv_(v.size())
	, v_(v.size())
	, ctx_(nullptr)
	, sock_(nullptr)
{
	for (size_t i=0;i<v.size();i++)
		sv_[i] = nullptr;
	int i = 0;
	for (const auto &s : v)
		m_.emplace(s, i++);
}

ChannelImpl::~ChannelImpl()
{
	for (void *sock : sv_)
		if (sock)
			zmq_close(sock);
	if (sock_)
		zmq_close(sock_);
	if (ctx_) {
		zmq_ctx_shutdown(ctx_);
		zmq_ctx_term(ctx_);
	}
}

bool ChannelImpl::Connect(const char *host, const std::map<key::Data, size_t> &output)
{
	const size_t kLength = 128;

	bool result = false;
	// get available address
	char address[64];
	auto *iflist = ziflist_new();
	assert(iflist);
	const char *name = ziflist_first(iflist);
	if (!name) {
		std::cerr << "failed to get interface name" << std::endl;
		ziflist_destroy(&iflist);
		return false;
	}
	std::sprintf(address, "%s", ziflist_address(iflist));
	ziflist_destroy(&iflist);

	ctx_ = zmq_ctx_new();
	assert(ctx_);

	sock_ = zmq_socket(ctx_, ZMQ_PUB);
	assert(sock_);
	char output_endpoint[kLength];
	std::sprintf(output_endpoint, "tcp://%s:*", address);
	int r = zmq_bind(sock_, output_endpoint);
	if (r != 0) {
		std::cerr << "failed to bind socket: "
				  << zmq_strerror(errno)
				  << std::endl;
		return false;
	}
	size_t len = kLength;
	r = zmq_getsockopt(sock_, ZMQ_LAST_ENDPOINT, &output_endpoint, &len);
	if (r != 0) {
		std::cerr << "failed to get last endpoint: "
				  << zmq_strerror(errno)
				  << std::endl;
		return false;
	}

	zactor_t *peer = zactor_new(zgossip, output_endpoint); // FIXME
	assert(peer);
	char endpoint[kLength];
	std::sprintf(endpoint, "tcp://%s:20010", host);
	zstr_sendx(peer, "CONNECT", endpoint, nullptr);
	// register output
	for (const auto &p : output) {
		const auto &kd = p.first;
		output_.emplace(kd.GetPrefixString(), p.second);
		char key[256];
		std::sprintf(key, "%s:%s", boost::uuids::to_string(kd.uuid).c_str(), kd.name.c_str());
		zstr_sendx(peer, "PUBLISH", key, output_endpoint, nullptr);
	}

	zpoller_t *poller = zpoller_new(peer, nullptr);
	assert(poller);
	// poll until all of input become registered
	while (!m_.empty()) {
		void *which = zpoller_wait(poller, -1); // no timeout
		if (!which)
			goto bail;
		assert(which == peer);
		char *command, *key, *value;
		zstr_recvx(peer, &command, &key, &value, nullptr);
		if (std::strcmp(command, "DELIVER") == 0) {
			std::cerr << "key: " << key << std::endl;
			key::Data kd;
			if (!key::Data::FromString(key, &kd)) {
				zstr_free(&command);
				zstr_free(&key);
				zstr_free(&value);
				goto bail;
			}
			auto it = m_.find(kd.GetPrefixString());
			if (it != m_.end()) {
				void *sock = zmq_socket(ctx_, ZMQ_SUB);
				assert(sock);
				r = zmq_setsockopt(sock, ZMQ_SUBSCRIBE, it->first.c_str(), kPrefixSize);
				assert(r == 0);
				r = zmq_connect(sock, value);
				if (r != 0) {
					std::cerr << "failed to connect socket: "
							  << zmq_strerror(errno)
							  << std::endl;
					return false;
				}
				sv_[it->second] = sock;
				m_.erase(it);
			}
		}
		zstr_free(&command);
		zstr_free(&key);
		zstr_free(&value);
	}

	// TODO: initial values
	for (auto &m : v_)
		m.emplace(0, 0);
	result = true;
 bail:
	zpoller_destroy(&poller);
	zactor_destroy(&peer);
	return result;
}

bool ChannelImpl::Lookup(int index, double t, double *d)
{
	auto &m = v_[index];
	std::pair<std::map<double, double>::iterator, std::map<double, double>::iterator> r;
	r = m.equal_range(t);
	if (r.first != r.second) { // found the exact data
		*d = r.first->second;
		return true;
	}
	if (r.first == m.begin()) { // smaller than every existing data
		*d = 0; // FIXME
		return true;
	}
	if (r.first == m.end()) { // larger than every existing data
		double t_r, v_r;
		while ( zmq_recv(sv_[index], buf_, kBufferSize, 0) >= 0 ) {
			std::memcpy(&t_r, buf_+kPrefixSize, sizeof(t_r));
			std::memcpy(&v_r, buf_+kPrefixSize+sizeof(t_r), sizeof(v_r));
			v_[index].emplace(t_r, v_r);
			std::cerr << "remote: " << std::string(buf_+16, kPrefixSize-16)
					  << ": " << t_r
					  << ": " << v_r
					  << std::endl;
			if (t_r >= t) {
				*d = v_r;
				return true;
			}
		}
		std::cerr << "failed to receive data: "
				  << zmq_strerror(errno) << std::endl;
		return false;
	}
	auto it = r.first;
	--it;
	*d = it->second; // FIXME
	return true;
}

void ChannelImpl::Send(const double *data)
{
	for (const auto &p : output_) {
		const auto &prefix = p.first;
		std::memset(buf_, 0, kBufferSize);
		std::memcpy(buf_, prefix.c_str(), kPrefixSize);
		std::memcpy(buf_+kPrefixSize, &data[kIndexTime], sizeof(double));
		std::memcpy(buf_+kPrefixSize+sizeof(double), &data[p.second], sizeof(double));
		if (zmq_send(sock_, buf_, kBufferSize, 0) < 0)
			std::cerr << "failed to send data: "
					  << zmq_strerror(errno)
					  << std::endl;
	}
}


Channel::Channel(const std::vector<std::string> &cv)
	: impl_(new ChannelImpl(cv))
{}

Channel::~Channel() = default;

bool Channel::Connect(const char *host, const std::map<key::Data, size_t> &output)
{
	return impl_->Connect(host, output);
}

bool Channel::Lookup(int index, double t, double *d)
{
	return impl_->Lookup(index, t, d);
}

void Channel::Send(const double *data)
{
	impl_->Send(data);
}

bool LoadChannel(sqlite3 *db,
				 std::unique_ptr<Channel> &channel)
{
	std::vector<std::string> v;
	char *em;
	int e = sqlite3_exec(db, "SELECT uuid, name FROM channels",
						 &Process, &v, &em);
	if (e != SQLITE_OK) {
		std::cerr << "failed to select channels: " << e << ": " << em << std::endl;
		sqlite3_free(em);
		return false;
	}
	if (v.empty()) {
		channel.reset();
		return true;
	}
	channel.reset(new Channel(v));
	return true;
}

}
}
