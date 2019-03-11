/* -*- Mode: C++; tab-width: 4; indent-tabs-mode: t; c-basic-offset: 4 -*- vim:set ts=4 sw=4 sts=4 noet: */
#ifdef HAVE_CONFIG_H
#include "config.h"
#endif

#include <cerrno>
#include <cstdio>
#include <cstdlib>
#include <cstring>
#include <iostream>
#include <memory>
#include <thread>

#include <fcntl.h>
#include <sys/stat.h>
#include <sys/types.h>
#include <unistd.h>

#include <boost/uuid/string_generator.hpp>
#include <czmq.h>
#include <zmq.h>

#include "fppp.h"
#include "fppp/utility.h"
#include "isdf/reader.h"

namespace {

flint::key::Data feed_data[2];

int feed_fd[2];

bool Prepare()
{
	static const char *kFile[2] = {"/dev/rtmotor_raw_l0", "/dev/rtmotor_raw_r0"};

	for (int i=0;i<2;i++) {
		int fd = open(kFile[i], O_WRONLY);
		if (fd == -1) {
			std::cerr << "failed to open " << kFile[i]
					  << ": " << std::strerror(errno)
					  << std::endl;
			return false;
		}
		feed_fd[i] = fd;
	}
	return true;
}

void Change(int i, int v)
{
	static char buf[32];

	int len = std::sprintf(buf, "%d", v);
	assert(len > 0);
	ssize_t r = write(feed_fd[i], buf, len);
	std::cerr << i << ": " << buf << std::endl;
	if (r < 0)
		std::cerr << "failed to write: "
				  << std::string(buf, len)
				  << std::endl;
}

void Drive(boost::uuids::uuid uuid, std::string name, const char *time, const char *value)
{
	(void)time;
	int v = static_cast<int>(*reinterpret_cast<const double *>(value));
	if (uuid == feed_data[0].uuid && name == feed_data[0].name) {
		Change(0, v);
	} else if (uuid == feed_data[1].uuid && name == feed_data[1].name) {
		Change(1, v);
	} else {
		assert(false);
	}
}

const char kUuid[] = "ed17af54-f415-11e6-a239-e39cfbb702cb";

const std::string kVariableNames[] = {"s0", "s1", "s2", "s3"};

bool Sense(double t, flint::fppp::Publisher *pub)
{
	const size_t kSensorBufferSize = 128;
	static char sensor_buf[kSensorBufferSize];
	static boost::uuids::string_generator gen;
	static const boost::uuids::uuid uuid = gen(kUuid);
	static char time_buf[sizeof(double)];
	static char value_buf[sizeof(double)];

	std::memcpy(time_buf, &t, sizeof(double));
	int fd = open("/dev/rtlightsensor0", O_RDONLY);
	if (fd == -1) {
		std::cerr << "failed to open /dev/rtlightsensor0: "
				  << std::strerror(errno)
				  << std::endl;
		return false;
	}
	ssize_t r = read(fd, sensor_buf, kSensorBufferSize);
	close(fd);
	if (r <= 6) {
		std::cerr << "failed to read sensor input" << std::endl;
		return false;
	}
	sensor_buf[r] = '\0';
	int i = 0;
	char *p = sensor_buf, *q;
	do {
		q = nullptr;
		auto n = std::strtol(p, &q, 10);
		if (n == LONG_MIN) {
			std::cerr << "got LONG_MIN" << std::endl;
			return false;
		}
		if (n == LONG_MAX) {
			std::cerr << "got LONG_MAX" << std::endl;
			return false;
		}
		double d = static_cast<double>(n);
		std::memcpy(value_buf, &d, sizeof(double));
		(*pub)(uuid, kVariableNames[i], time_buf, value_buf);
		p = q;
	} while (q && ++i < 4);
	return (i == 4);
}

void Usage()
{
	std::cerr << "usage: fppp-rtm HOST IN0 IN1" << std::endl;
}

}

int main(int argc, char *argv[])
{
	if (argc < 4) {
		Usage();
		return EXIT_FAILURE;
	}

	void *ctx = zmq_ctx_new();
	flint::fppp::ContextGuard g(ctx);
	std::set<flint::key::Data> in;
	boost::uuids::string_generator gen;
	for (int i=2;i<4;i++) {
		flint::key::Data kd;
		if (!flint::key::Data::FromString(argv[i], &kd)) {
			std::cerr << "invalid input name: " << argv[i] << std::endl;
			return EXIT_FAILURE;
		}
		in.insert(kd);
		feed_data[i-2] = kd;
	}
	std::vector<flint::key::Data> out;
	for (const auto &name : kVariableNames) {
		flint::key::Data kd;
		kd.uuid = gen(kUuid);
		kd.name = name;
		out.push_back(kd);
	}
	flint::fppp::Publisher *pub = nullptr;
	flint::fppp::Subscriber *sub = nullptr;
	zactor_t *peer = flint::fppp::ShakeHands(ctx, argv[1], in, out, &pub, &sub);
	if (!peer)
		return EXIT_FAILURE;
	assert(pub && sub);
	if (Prepare()) {
		std::thread th([sub]{(*sub)(&Drive);});
		th.detach();
		auto s = zclock_mono();
		auto t = s;
		for (;;) {
			Sense(static_cast<double>(t-s)/1000, pub);
			zclock_sleep(100);
			t = zclock_mono();
		}
	}
	close(feed_fd[0]);
	close(feed_fd[1]);
	zactor_destroy(&peer);
	return EXIT_SUCCESS;
}
