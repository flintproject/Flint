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
#include "flint/background.h"
#include "flint/bc.h"
#include "job.h"
#include "lo/layout.h"
#include "lo/layout_loader.h"
#include "load.h"
#include "run/spec.h"
#include "runtime/flow.h"
#include "system.h"
#include "task.h"
#include "task/config-reader.h"
#include "utf8path.h"

namespace flint {
namespace tr {

namespace {

bool PrintFileAsByteArray(const char *filename, const char *name,
						  std::ostream *os)
{
	static const int kBufferSize = 1024;

	boost::filesystem::path path(filename);
	size_t size = static_cast<size_t>(boost::filesystem::file_size(path));
	FILE *fp = std::fopen(filename, "rb");
	if (!fp) {
		std::perror(filename);
		return false;
	}
	std::unique_ptr<unsigned char[]> buf(new unsigned char[kBufferSize]);
	*os << std::endl;
	*os << "static const size_t " << name << "_length = " << size << ';' << std::endl;
	*os << "static const unsigned char " << name << '[' << size << "] = {" << std::endl;
	size_t t = 0;
	boost::io::ios_flags_saver saver(*os);
	while (t < size) {
		size_t s = std::fread(buf.get(), 1, kBufferSize, fp);
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
	auto e = std::ferror(fp);
	std::fclose(fp);
	if (e) {
		std::cerr << "an error occurred when reading " << filename << std::endl;
		return false;
	}
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

bool Translate(const cli::RunOption &option)
{
	std::vector<double> data;
	if (!load::Load(option.model_filename().c_str(), load::kRun, 0, &data))
		return false;

	db::Driver driver("model.db");
	sqlite3 *db = driver.db();
	// prepare spec.txt in both cases
	if (option.has_spec_filename()) {
		boost::filesystem::path spec_path = GetPathFromUtf8(option.spec_filename().c_str());
		boost::system::error_code ec;
		if (!boost::filesystem::is_regular_file(spec_path, ec) || ec) {
			std::cerr << ec.message() << std::endl;
			return false;
		}
		boost::filesystem::copy_file(spec_path, "spec.txt", ec);
		if (ec) {
			std::cerr << "failed to copy "
				 << option.spec_filename()
				 << " to spec.txt: "
				 << ec.message()
				 << std::endl;
			return false;
		}
	} else {
		// create the list of all variables
		FILE *fp = fopen("spec.txt", "w");
		if (!fp) {
			std::perror("spec.txt");
			return false;
		}
		if (!run::Spec(db, fp))
			return false;
		fclose(fp);
	}
	task::ConfigReader reader(db);
	if (!reader.Read())
		return false;
	if (!filter::Create(db, "spec.txt", "layout", "filter"))
		return false;
	if (!filter::Isdh("filter", "isdh"))
		return false;
	if (reader.GetMethod() != compiler::Method::kArk) {
		cas::DimensionAnalyzer da;
		if (!da.Load(db))
			return false;
		compiler::Compiler c(&da);
		if (!c.Compile(db, "input_eqs", reader.GetMethod(), "bc"))
			return false;
	}
	boost::filesystem::path output_path = GetPathFromUtf8(option.output_filename().c_str());
	std::string output_file = output_path.string();

	// load layout at first
	std::unique_ptr<Layout> layout(new Layout);
	{
		std::unique_ptr<LayoutLoader> loader(new LayoutLoader("layout"));
		if (!loader->Load(layout.get()))
			return false;
	}
	size_t layer_size = layout->Calculate();
	assert(layer_size > kOffsetBase);

	// load filter next
	std::unique_ptr<filter::Writer> writer;
	{
		filter::Cutter cutter;
		if (!cutter.Load("filter", layer_size)) return false;
		writer.reset(cutter.CreateWriter());
	}

	std::ofstream ofs(output_file);
	if (!ofs) {
		std::cerr << "failed to open output file: " << output_file << std::endl;
		return false;
	}

	// load bc next
	int nol = 0;
	std::unique_ptr<Translator> translator(new Translator(layout.get(), layer_size, ofs));
	if (!LoadBytecode("bc", &nol, translator.get()))
		return false;
	if (nol <= 0) {
		std::cerr << "invalid nol: " << nol << std::endl;
		return false;
	}

	// replace nominal location in bytecode
	if (!translator->SolveLocation())
		return false;
	translator->CalculateCodeOffset();

	std::unique_ptr<FlowInboundMap> inbound(new FlowInboundMap);
	if (!LoadFlows(db, inbound.get()))
		return false;
	if (!translator->SolveDependencies(nol, inbound.get()))
		return false;

	translator->PrintHeader(nol, layer_size, reader.length(), reader.step());

	// TODO: use std::hexfloat once GCC 5 is supposed to be available
	// choose the defaultfloat
	ofs.unsetf(std::ios::floatfield);
	// See Theorem 15 of <http://docs.oracle.com/cd/E19957-01/806-3568/ncg_goldberg.html>.
	ofs.precision(17);
	translator->PrintFunctions();
	translator->PrintReductionFunctions();
	if (!PrintFileAsByteArray("isdh", "isdh", &ofs))
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
