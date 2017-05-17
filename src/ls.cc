/* -*- Mode: C++; tab-width: 4; indent-tabs-mode: t; c-basic-offset: 4 -*- vim:set ts=4 sw=4 sts=4 noet: */
#ifdef HAVE_CONFIG_H
#include "config.h"
#endif

#include "flint/ls.h"

#include <cassert>
#include <cmath>
#include <cstdio>
#include <cstdlib>
#include <cstring>
#include <iostream>
#include <limits>
#include <map>
#include <memory>
#include <string>

#include <boost/uuid/nil_generator.hpp>
#include <boost/uuid/uuid.hpp>

#include "bc/index.h"
#include "flint/dps.h"
#include "flint/key.h"
#include "isdf/reader.h"
#include "lo/layout.h"
#include "utf8path.h"

namespace flint {
namespace ls {

namespace {

class Handler {
public:
	explicit Handler(std::map<int, key::Data> &m)
		: m_(m)
	{}

	void GetDescription(std::uint32_t i, std::uint32_t num_bytes, const char *d) {
		if (i == 0) // skip the first column
			return;
		key::Data kd;
		if (num_bytes < 38) {
			kd.uuid = boost::uuids::nil_uuid();
			kd.name = std::string(d, num_bytes);
		} else {
			// TODO: check returned value
			(void)key::Data::FromString(std::string(d, num_bytes), &kd);
		}
		m_.emplace(i, kd);
	}

private:
	std::map<int, key::Data> &m_;
};

}

std::unique_ptr<Configuration> CreateConfiguration(const char *dps_path, const Layout &layout)
{
	std::unique_ptr<Configuration> config;
	boost::filesystem::path path = GetPathFromUtf8(dps_path);
	std::map<int, key::Data> m_dps;
	{
		boost::filesystem::ifstream ifs(path, std::ios::in|std::ios::binary);
		if (!ifs)
			return config;
		isdf::Reader reader;
		if (!reader.ReadHeader(&ifs))
			return config;
		if (!reader.SkipComment(&ifs))
			return config;
		Handler handler(m_dps);
		if (!reader.ReadDescriptions(handler, &ifs))
			return config;
		ifs.close();
		config.reset(new Configuration);
		config->data_offset = reader.GetDataOffset();
		config->row_size = reader.num_objs() * sizeof(double);
	}
	std::map<key::Data, size_t> m_data;
	for (const auto &p : m_dps)
		m_data.emplace(p.second, 0);
	if (!layout.SelectByKeyData(&m_data)) {
		config.reset();
		return config;
	}
	for (const auto &p : m_dps) {
		auto it = m_data.find(p.second);
		if (it == m_data.end()) // ignore missing data points
			continue;
		if (it->second == 0) // found in data points, but not found in the layout
			continue;
		auto r = config->indice.emplace(p.first, it->second);
		assert(r.second);
	}
	auto path_s = path.string();
	boost::interprocess::file_mapping fm(path_s.c_str(), boost::interprocess::read_only);
	config->fm = std::move(fm);
	config->bound = std::numeric_limits<double>::max();
	return config;
}

Accumulator::Accumulator(Configuration &config)
	: config_(config)
	, cursor_(config.fm, config.data_offset, config.row_size)
	, sum_(0)
{}

Accumulator::State Accumulator::operator()(const double *data)
{
	double *p;
	switch (cursor_(data[kIndexTime], &p)) {
	case dps::Cursor::Position::kGt:
		return State::kLeq;
	case dps::Cursor::Position::kLeq:
		{
			// access the mapped region sequentially
			for (const auto &idx : config_.indice)
				sum_ += std::pow(p[idx.first] - data[idx.second], 2);
			std::lock_guard<std::mutex> g(config_.mutex);
			if (config_.bound < sum_)
				return State::kGt;
			return State::kLeq;
		}
	default:
		{
			std::lock_guard<std::mutex> g(config_.mutex);
			if (sum_ < config_.bound)
				config_.bound = sum_;
			return State::kDone;
		}
	}
}

}
}
