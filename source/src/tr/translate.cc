/* -*- Mode: C++; tab-width: 4; indent-tabs-mode: t; c-basic-offset: 4 -*- vim:set ts=4 sw=4 sts=4 noet: */
#ifdef HAVE_CONFIG_H
#include "config.h"
#endif

#include "flint/tr.h"

#include <cassert>
#include <cstdio>
#include <cstdlib>
#include <cstring>
#include <iomanip>
#include <iostream>
#include <memory>

#define BOOST_FILESYSTEM_NO_DEPRECATED
#include <boost/filesystem.hpp>
#include <boost/io/ios_state.hpp>

#include "bc/index.h"
#include "cas/dimension.h"
#include "tr/translator.h"
#include "compiler.h"
#include "db/driver.h"
#include "db/statement-driver.h"
#include "file.h"
#include "filter.h"
#include "filter/cutter.h"
#include "flint/bc.h"
#include "flint/ls.h"
#include "flint/numeric.h"
#include "flint/utf8path.h"
#include "job.h"
#include "lo/layout.h"
#include "lo/layout_loader.h"
#include "load.h"
#include "run/spec.h"
#include "runtime/flow.h"
#include "task.h"
#include "task/config-reader.h"

namespace flint {
namespace tr {

namespace {

bool PrintFileAsByteArray(const boost::filesystem::path &path, const char *name,
						  std::ostream *os)
{
	const size_t kBufferSize = 8192;

	size_t size = static_cast<size_t>(boost::filesystem::file_size(path));
	boost::filesystem::ifstream ifs(path, std::ios::in|std::ios::binary);
	if (!ifs) {
		std::cerr << "failed to open " << path << std::endl;
		return false;
	}
	std::unique_ptr<unsigned char[]> buf(new unsigned char[kBufferSize]);
	*os << std::endl;
	*os << "static const size_t " << name << "_length = " << size << ';' << std::endl;
	*os << "static const unsigned char " << name << '[' << size << "] = {" << std::endl;
	size_t t = 0;
	boost::io::ios_flags_saver saver(*os);
	while (t < size) {
		size_t s = std::min(size - t, kBufferSize);
		if (!ifs.read(reinterpret_cast<char *>(buf.get()), s)) {
			ifs.close();
			std::cerr << "an error occurred when reading " << path << std::endl;
			return false;
		}
		for (size_t i=0;i<s;i++) {
			os->put(((i&0xF) == 0x0) ? '\t' : ' ');
			*os << "0x"
				<< std::hex
				<< std::noshowbase
				<< std::setw(2)
				<< std::setfill('0')
				<< static_cast<unsigned int>(buf[i])
				<< ',';
			if ((i&0xF) == 0xF)
				*os << std::endl;
		}
		t += s;
	}
	ifs.close();
	if (size%kBufferSize != 0)
		*os << std::endl;
	*os << "};" << std::endl;
	return true;
}

bool PrintDataAsDoubleArray(const std::vector<double> &data,
							const char *name, std::ostream *os)
{
	*os << std::endl;
	*os << "static const double " << name << "[] = {" << std::endl;
	for (auto d : data)
		*os << '\t' << d << ',' << std::endl;
	*os << "};" << std::endl;
	return true;
}

}

bool Translate(const cli::RunOption &option, const boost::filesystem::path &dir)
{
	std::vector<double> data;
	std::unique_ptr<task::Task> task(load::Load(option.model_filename().c_str(), load::kRun, dir, &data));
	if (!task)
		return false;

	auto driver = db::Driver::Create(dir / "model.db");
	sqlite3 *db = driver->db();
	if (!db)
		return false;
	// prepare spec.txt in both cases
	if (option.has_spec_filename()) {
		boost::filesystem::path spec_path = GetPathFromUtf8(option.spec_filename().c_str());
		if (spec_path.empty())
			return false;
		boost::system::error_code ec;
		if (!boost::filesystem::is_regular_file(spec_path, ec) || ec) {
			std::cerr << ec.message() << std::endl;
			return false;
		}
		boost::filesystem::copy_file(spec_path, dir / "spec.txt", ec);
		if (ec) {
			std::cerr << "failed to copy "
					  << option.spec_filename()
					  << " to "
					  << dir
					  << "/spec.txt: "
					  << ec.message()
					  << std::endl;
			return false;
		}
	} else {
		// create the list of all variables
		boost::filesystem::ofstream ofs(dir / "spec.txt", std::ios::out|std::ios::binary);
		if (!ofs) {
			std::cerr << "failed to open "
					  << dir
					  << "/spec.txt"
					  << std::endl;
			return false;
		}
		bool b = run::Spec(db, &ofs);
		ofs.close();
		if (!b)
			return false;
	}
	task::ConfigReader reader(db);
	if (!reader.Read())
		return false;
	if (!filter::Create(db,
						dir / "spec.txt",
						dir / "layout",
						dir / "filter"))
		return false;
	if (!filter::Isdh(dir / "filter", dir / "isdh"))
		return false;
	// TODO: granularity and output_start_time
	if (reader.GetMethod() != compiler::Method::kArk) {
		cas::DimensionAnalyzer da;
		if (!da.Load(db))
			return false;
		compiler::Compiler c(&da);
		task->bc.reset(c.Compile(db, "input_eqs", reader.GetMethod()));
		if (!task->bc)
			return false;
	}
	boost::filesystem::path output_path = GetPathFromUtf8(option.output_filename().c_str());
	if (output_path.empty())
		return false;
	std::string output_file = output_path.string();

	// load layout at first
	std::unique_ptr<Layout> layout(new Layout);
	{
		std::unique_ptr<LayoutLoader> loader(new LayoutLoader(dir / "layout"));
		if (!loader->Load(layout.get()))
			return false;
	}
	size_t layer_size = layout->Calculate();
	assert(layer_size > kOffsetBase);

	// load filter next
	std::unique_ptr<filter::Writer> writer;
	{
		filter::Cutter cutter;
		if (!cutter.Load(dir / "filter", layer_size)) return false;
		writer.reset(cutter.CreateWriter());
	}

	std::ofstream ofs(output_file);
	if (!ofs) {
		std::cerr << "failed to open output file: " << output_file << std::endl;
		return false;
	}

	int nol = task->bc->nol;
	std::unique_ptr<Translator> translator(new Translator(layout.get(), layer_size, task->bc.get(), ofs));
	if (nol <= 0) {
		std::cerr << "invalid nol: " << nol << std::endl;
		return false;
	}

	// replace nominal location in bytecode
	if (!translator->SolveLocation())
		return false;
	translator->CalculateCodeOffset();

	if (!translator->SolveDependencies(&task->inbound))
		return false;

	translator->PrintHeader(layer_size, reader.length(), reader.step());

	// TODO: use std::hexfloat once GCC 5 is supposed to be available
	RequestMaxNumOfDigitsForDouble(ofs);
	translator->PrintFunctions();
	translator->PrintReductionFunctions();
	if (!PrintFileAsByteArray(dir / "isdh", "isdh", &ofs))
		return false;
	if (!PrintDataAsDoubleArray(data,
								"input", &ofs))
		return false;
	translator->PrintMain(*writer);

	ofs.close();
	return bool(ofs);
}

}
}
