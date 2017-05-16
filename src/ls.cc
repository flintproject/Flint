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
#include "fppp.h"
#include "isdf/reader.h"
#include "utf8path.h"

namespace flint {
namespace ls {

Configuration::Configuration()
	: bound(std::numeric_limits<double>::max())
{}

namespace {

class Handler {
public:
	explicit Handler(std::map<int, fppp::KeyData> &m)
		: m_(m)
	{}

	void GetDescription(std::uint32_t i, std::uint32_t num_bytes, const char *d) {
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
		m_.emplace(i, kd);
	}

private:
	std::map<int, fppp::KeyData> &m_;
};

}

std::unique_ptr<Configuration> CreateConfiguration(const char *dps_path, const Layout &layout)
{
	std::unique_ptr<Configuration> config(new Configuration); // FIXME
	config->filename = dps_path; // FIXME: UTF-8 to native

	boost::filesystem::path path = GetPathFromUtf8(dps_path);
	boost::filesystem::ifstream ifs(path, std::ios::in|std::ios::binary);
	if (!ifs) {
		config.reset();
		return config;
	}
	isdf::Reader reader;
	if (!reader.ReadHeader(&ifs)) {
		config.reset();
		return config;
	}
	config->data_offset = reader.GetDataOffset();
	config->row_size = reader.num_objs() * sizeof(double);
	if (!reader.SkipComment(&ifs)) {
		config.reset();
		return config;
	}
	std::map<int, fppp::KeyData> m_dps;
	Handler handler(m_dps);
	if (!reader.ReadDescriptions(handler, &ifs)) {
		config.reset();
		return config;
	}
	std::map<fppp::KeyData, size_t> m_data;
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
	return config;
}

Accumulator::Accumulator(Configuration &config)
	: config_(config)
	, cursor_(boost::interprocess::file_mapping(config.filename, boost::interprocess::read_only), config.data_offset, config.row_size)
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
