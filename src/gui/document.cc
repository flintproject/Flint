/* -*- Mode: C++; tab-width: 4; indent-tabs-mode: t; c-basic-offset: 4 -*- vim:set ts=4 sw=4 sts=4 noet: */
#ifdef HAVE_CONFIG_H
#include "config.h"
#endif

#include "gui/document.h"

#include <cassert>
#include <cstdio>
#include <cstdlib>
#include <fstream>
#include <iostream>
#include <memory>

#include "bc/index.h"
#include "bc/pack.h"
#include "db/read-only-driver.h"
#include "phml.pb.h"

namespace flint {
namespace gui {

Document::Document(int id, const wxString &path, std::vector<double> data)
	: id_(id)
	, path_(path)
	, data_(std::move(data))
{
}

const char *Document::GetFormat() const
{
	switch (format_) {
	case file::kCellml:
		return "cellml";
	case file::kPhml:
		return "phml";
	case file::kSbml:
		return "sbml";
	default:
		assert(false);
		return nullptr;
	}
}

bool Document::Load()
{
	if (!LoadFileFormat())
		return false;

	std::unique_ptr<char[]> buf(new char[64]); // large enough
	std::sprintf(buf.get(), "%d/model.db", id_);
	db::ReadOnlyDriver driver(buf.get());
	auto db = driver.db();
	if (!db)
		return false;
	if (!LoadUnitOfTime(db))
		return false;
	if (choices_time_.empty())
		choices_time_.push_back("unit time");
	choices_method_.push_back("Euler");
	choices_method_.push_back("Runge Kutta");
	choices_method_.push_back("ARK");

	// initialize configuration
	initial_config_.method = "Runge Kutta";
	initial_config_.length = "100";
	initial_config_.length_unit = 0;
	initial_config_.step = "0.01";
	initial_config_.step_unit = 0;
	initial_config_.start = 0;
	initial_config_.start_unit = 0;
	initial_config_.granularity = 1;
	initial_config_.filter_pattern = "Wildcard";
	initial_config_.filter_value = "*";

	return LoadNc() && LoadParam() && LoadVar();
}

bool Document::LoadFileFormat()
{
	std::unique_ptr<char[]> buf(new char[64]); // large enough
	std::sprintf(buf.get(), "%d/file.txt", id_);
	std::ifstream ifs(buf.get());
	if (!ifs) {
		wxLogError("failed to open %s", buf.get());
		return false;
	}
	ifs.getline(buf.get(), 64);
	auto c = ifs.gcount();
	if (c <= 0) {
		wxLogError("failed to read %s", buf.get());
		return false;
	}
	ifs.close();
	std::string s(buf.get());
	if (s == "cellml") {
		format_ = file::kCellml;
		return true;
	}
	if (s == "isml" || s == "phml" || s == "phz") {
		format_ = file::kPhml;
		return true;
	}
	if (s == "sbml") {
		format_ = file::kSbml;
		return true;
	}
	wxLogError("unknown file format: %s", s);
	return false;
}

bool Document::LoadNc()
{
	std::unique_ptr<char[]> buf(new char[64]); // large enough
	std::sprintf(buf.get(), "%d/nc", id_);
	std::ifstream ifs(buf.get(), std::ios::binary);
	if (!ifs) {
		wxLogError("failed to open %s", buf.get());
		return false;
	}
	phml::NumericalConfiguration nc;
	if (nc.ParseFromIstream(&ifs)) {
		if (nc.has_td()) {
			initial_config_.step = nc.td().step();
			auto it = ids_time_.find(nc.td().unit_id());
			assert(it != ids_time_.end());
			initial_config_.step_unit = it->second;
		}
		if (nc.has_rg() && nc.rg().has_seed()) {
			// TODO
		}
		if (nc.has_integration()) {
			if (nc.integration() == "ark") {
				initial_config_.method = "ARK";
			} else if (nc.integration() == "euler") {
				initial_config_.method = "Euler";
			} else if (nc.integration() == "4th-rungekutta") {
				initial_config_.method = "Runge Kutta";
			}
		}
		if (nc.has_sts()) {
			initial_config_.length = nc.sts().value();
			auto it = ids_time_.find(nc.sts().unit_id());
			assert(it != ids_time_.end());
			initial_config_.length_unit = it->second;
		} else if (nc.has_td()) {
			// default to step's unit
			initial_config_.length_unit = initial_config_.step_unit;
		}
		initial_config_.start_unit = initial_config_.length_unit;
	}
	ifs.close();
	return true;
}

bool Document::LoadParam()
{
	std::unique_ptr<char[]> buf(new char[64]); // large enough
	std::sprintf(buf.get(), "%d/param", id_);
	std::ifstream ifs(buf.get(), std::ios::binary);
	if (!ifs) {
		wxLogError("failed to open %s", buf.get());
		return false;
	}
	lo::Header header;
	if (!UnpackFromIstream(header, &ifs)) {
		ifs.close();
		return false;
	}
	int size = kOffsetBase;
	lo::Column column;
	while (UnpackFromIstream(column, &ifs)) {
		size += column.col() * column.row();
		param_.push_back(column);
	}
	ifs.close();
	return size <= header.size(); // TODO
}

bool Document::LoadVar()
{
	std::unique_ptr<char[]> buf(new char[64]); // large enough
	std::sprintf(buf.get(), "%d/var", id_);
	std::ifstream ifs(buf.get(), std::ios::binary);
	if (!ifs) {
		wxLogError("failed to open %s", buf.get());
		return false;
	}
	lo::Header header;
	if (!UnpackFromIstream(header, &ifs)) {
		ifs.close();
		return false;
	}
	int size = kOffsetBase;
	lo::Column column;
	while (UnpackFromIstream(column, &ifs)) {
		size += column.col() * column.row();
		var_.push_back(column);
	}
	ifs.close();
	return size <= header.size(); // TODO
}

}
}
