/* -*- Mode: C++; tab-width: 4; indent-tabs-mode: t; c-basic-offset: 4 -*- vim:set ts=4 sw=4 sts=4 noet: */
#ifdef HAVE_CONFIG_H
#include "config.h"
#endif

#include <cstdio>
#include <cstdlib>
#include <cstring>
#include <fstream>
#include <iostream>
#include <thread>
#include <utility>
#include <vector>

#include <boost/uuid/nil_generator.hpp>
#include <boost/uuid/uuid_io.hpp>
#include <czmq.h>
#include <zmq.h>

#include "fppp.h"
#include "fppp/utility.h"
#include "isdf/reader.h"

namespace {

void Print(boost::uuids::uuid uuid, std::string name, const char *time, const char *value)
{
	std::cout << uuid
			  << ":" << name
			  << "| " << *reinterpret_cast<const double *>(time)
			  << ", " << *reinterpret_cast<const double *>(value)
			  << std::endl;
}

struct Handler
{
	void GetDescription(std::uint32_t i, std::uint32_t num_bytes, const char *d)
	{
		if (i == 0) // skip the first column
			return;
		flint::fppp::KeyData kd;
		if (num_bytes < 38) {
			kd.uuid = boost::uuids::nil_uuid();
			kd.name = std::string(d, num_bytes);
		} else {
			// TODO: check returned value
			(void)flint::fppp::KeyData::FromString(std::string(d, num_bytes), &kd);
		}
		kdv.push_back(kd);
		iv.push_back(i * sizeof(double));
	}

	int GetStep(size_t /*size*/, const char *buf) const {
		std::int64_t td = static_cast<std::int64_t>(*reinterpret_cast<const double *>(buf) * 1000)+t-zclock_mono();
		zclock_sleep(td);
		for (size_t i=0;i<kdv.size();i++) {
			(*pub)(kdv[i].uuid, kdv[i].name, buf, buf+iv[i]);
		}
		return 1;
	}

	std::vector<flint::fppp::KeyData> kdv;
	std::vector<size_t> iv;
	flint::fppp::Publisher *pub;
	flint::fppp::Subscriber *sub;
	std::int64_t t;
};

void Usage()
{
	std::cerr << "usage: fppp-isd FILE HOST IN [...]" << std::endl;
}

}

int main(int argc, char *argv[])
{
	if (argc < 4) {
		Usage();
		return EXIT_FAILURE;
	}
	flint::isdf::Reader reader;
	std::ifstream ifs(argv[1], std::ios::in|std::ios::binary);
	if (!ifs.is_open()) {
		std::cerr << "failed to open " << argv[1] << std::endl;
		return EXIT_FAILURE;
	}
	if (!reader.ReadHeader(&ifs))
		return EXIT_FAILURE;
	Handler handler;
	if (!reader.ReadDescriptions(handler, &ifs))
		return EXIT_FAILURE;
	if (!reader.SkipComment(&ifs))
		return EXIT_FAILURE;
	if (!reader.ReadUnits(&ifs))
		return EXIT_FAILURE;

	void *ctx = zmq_ctx_new();
	flint::fppp::ContextGuard g(ctx);
	// FIXME
	std::set<flint::fppp::KeyData> in;
	for (int i=3;i<argc;i++) {
		flint::fppp::KeyData kd;
		if (!flint::fppp::KeyData::FromString(argv[i], &kd)) {
			std::cerr << "invalid input name: " << argv[i] << std::endl;
			return false;
		}
		in.insert(kd);
	}
	zactor_t *peer = flint::fppp::ShakeHands(ctx, argv[2], in, handler.kdv, &handler.pub, &handler.sub);
	if (!peer)
		return EXIT_FAILURE;
	assert(handler.pub && handler.sub);
	{
		std::thread th([&handler]{(*handler.sub)(&Print);});
		handler.t = zclock_mono();
		if (!reader.ReadSteps(handler, &ifs))
			return EXIT_FAILURE;
		delete handler.pub;
		ifs.close();
		std::cerr << "read all steps." << std::endl;
		th.join();
	}
	zactor_destroy(&peer);
	return EXIT_SUCCESS;
}
