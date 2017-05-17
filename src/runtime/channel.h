/* -*- Mode: C++; tab-width: 4; indent-tabs-mode: t; c-basic-offset: 4 -*- vim:set ts=4 sw=4 sts=4 noet: */
#ifndef FLINT_RUNTIME_CHANNEL_H_
#define FLINT_RUNTIME_CHANNEL_H_

#include <map>
#include <memory>
#include <string>
#include <vector>

#include "sqlite3.h"

#include "fppp.h"

namespace flint {
namespace runtime {

class ChannelImpl;

class Channel {
public:
	Channel(const Channel &) = delete;
	Channel &operator=(const Channel &) = delete;

	explicit Channel(const std::vector<std::string> &cv);

	~Channel();

	bool Connect(const char *host, const std::map<key::Data, size_t> &output);

	bool Lookup(int index, double t, double *d);

	void Send(const double *data);

private:
	std::unique_ptr<ChannelImpl> impl_;
};

bool LoadChannel(sqlite3 *db,
				 const char *host,
				 const std::map<key::Data, size_t> &output,
				 std::unique_ptr<Channel> &channel);

}
}

#endif
