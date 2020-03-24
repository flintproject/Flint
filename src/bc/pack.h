/* -*- Mode: C++; tab-width: 4; indent-tabs-mode: t; c-basic-offset: 4 -*- vim:set ts=4 sw=4 sts=4 noet: */
#ifndef FLINT_BC_PACK_H_
#define FLINT_BC_PACK_H_

#ifdef HAVE_ARPA_INET_H
#include <arpa/inet.h>
#else
#include <winsock2.h>
#endif
#include <cassert>
#include <cstdint>
#include <cstdio>
#include <cstring>
#include <iostream>
#include <limits>
#include <memory>

namespace flint {

template<typename TMessage>
bool PackToOstream(const TMessage &message, std::ostream *os)
{
	const size_t kHeadSize = 4;
	char buffer[kHeadSize];

	size_t bs = message.ByteSizeLong();
	assert(bs > 0);
	if (std::numeric_limits<uint32_t>::max() < bs) {
		std::cerr << "message is too big: " << bs << std::endl;
		return false;
	}
	uint32_t byte_size = static_cast<uint32_t>(bs);
	uint32_t n_byte_size = htonl(byte_size);
	memcpy(buffer, &n_byte_size, kHeadSize);
	if (!os->write(buffer, kHeadSize).good()) return false;
	return message.SerializeToOstream(os);
}

template<typename TMessage>
bool UnpackFromIstream(TMessage &message, std::istream *is)
{
	const size_t kHeadSize = 4;
	char buffer[kHeadSize];

	if (!is->read(buffer, kHeadSize).good()) return false;
	uint32_t n_byte_size = 0;
	memcpy(&n_byte_size, buffer, kHeadSize);
	uint32_t byte_size = ntohl(n_byte_size);
	if (byte_size == 0) {
		std::cerr << "found invalid message size: 0" << std::endl;
		return false;
	}
	std::unique_ptr<char[]> array(new char[byte_size]);
	if (!is->read(array.get(), byte_size).good()) return false;
	return message.ParseFromArray(array.get(), byte_size);
}

}

#endif
