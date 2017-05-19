/* -*- Mode: C++; tab-width: 4; indent-tabs-mode: t; c-basic-offset: 4 -*- vim:set ts=4 sw=4 sts=4 noet: */
#ifdef HAVE_CONFIG_H
#include "config.h"
#endif

#include <cstdio>
#include <cstdlib>
#include <cstring>
#include <fstream>
#include <iostream>

#include <boost/uuid/string_generator.hpp>
#include <boost/uuid/nil_generator.hpp>
#include <boost/uuid/uuid_io.hpp>
#include <czmq.h>

int main()
{
	char name[32];
	std::sprintf(name, "fppp-sup");
	zactor_t *sup = zactor_new(zgossip, name);
	assert(sup);
	zstr_sendx(sup, "BIND", "tcp://*:20010", nullptr);
	zstr_sendx(sup, "PORT", nullptr);
	char *command, *port_str;
	zstr_recvx(sup, &command, &port_str, nullptr);
	assert(std::strcmp(command, "PORT") == 0);
	std::cerr << "port: " << port_str << std::endl;
	zstr_free(&command);
	zstr_free(&port_str);

	zpoller_t *poller = zpoller_new(sup, nullptr);
	assert(poller);
	for (;;) {
		void *which = zpoller_wait(poller, -1); // no timeout
		if (!which) {
			// destroyed context or SIGINT
			break;
		}
		assert(which == sup);
		char *key, *value;
		zstr_recvx(sup, &command, &key, &value, nullptr);
		if (std::strcmp(command, "DELIVER") == 0) {
			std::cerr << "DELIVER: "
					  << key
					  << " -> "
					  << value
					  << std::endl;
			zstr_free(&key);
			zstr_free(&value);
		}
	}
	zpoller_destroy(&poller);
	zactor_destroy(&sup);
	return EXIT_SUCCESS;
}
