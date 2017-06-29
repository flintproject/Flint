/* -*- Mode: C++; tab-width: 4; indent-tabs-mode: t; c-basic-offset: 4 -*- vim:set ts=4 sw=4 sts=4 noet: */
#ifdef HAVE_CONFIG_H
#include "config.h"
#endif

#include <cerrno>
#include <cstdio>
#include <cstdlib>
#include <cstring>
#include <iostream>
#include <iomanip>
#include <fstream>
#include <memory>
#include <string>
#include <sstream>

#ifdef _WIN32
#include <process.h>
#include <windows.h>
#endif

#include <boost/program_options.hpp>

#include "flint/temporary-file.h"
#include "isd2csv.h"
#include "isdstrip.h"
#include "isdf/isdf.h"

namespace po = boost::program_options;

using namespace flint;

namespace {

bool ReadHeader(std::istream *is, char *header)
{
	is->read(header, sizeof(isdf::ISDFHeader));
	return is->good();
}

bool ReadDescriptions(std::uint32_t num_bytes_descs, std::istream *is, char *rest)
{
	is->read(rest, num_bytes_descs);
	return is->good();
}

bool CountColumns(const boost::filesystem::path &input, std::uint32_t *num_columns)
{
	boost::filesystem::ifstream ifs(input, std::ios::in|std::ios::binary);
	if (!ifs.is_open()) {
		std::cerr << "could not open input file: " << input << std::endl;
		return false;
	}
	std::unique_ptr<char[]> header(new char[sizeof(isdf::ISDFHeader)]);
	if (!ReadHeader(&ifs, header.get())) {
		std::cerr << "could not read header: " << input << std::endl;
		ifs.close();
		return false;
	}
	*num_columns = reinterpret_cast<isdf::ISDFHeader *>(header.get())->num_objs;
	ifs.close();
	return true;
}

int CallIsd2csv(const isd2csv::Option &option, const boost::filesystem::path &input, std::ostream *os)
{
	boost::filesystem::ifstream ifs(input, std::ios::in|std::ios::binary);
	if (!ifs) {
		std::cerr << "failed to open input file: " << input << std::endl;
		return EXIT_FAILURE;
	}
	int r = isd2csv::Convert(option, &ifs, os);
	ifs.close();
	return r;
}

template<typename TChar>
void PutQuotedPath(const TChar *path, std::ostringstream *bss)
{
	*bss << "\"";
	TChar c;
	while ( (c = *path++) ) {
		switch (c) {
		case '\\':
			*bss << "\\\\";
			break;
		default:
			*bss << c;
			break;
		}
	}
	*bss << "\"";
}

template<typename TChar>
void CreateScript(std::uint32_t num_columns,
				  const TChar *csv_path,
				  const char *output_path,
				  std::ostringstream *bss)
{
	unsigned int row = 1, col = 1;
	unsigned int w = 640, h = 480;

	num_columns -= 1; // excluding the first "time"
	if (num_columns >= 16) {
		row = col = 4;
		w *= 2;
		h *= 2;
	} else if (num_columns >= 9) {
		row = col = 3;
		w *= 2;
		h *= 2;
	} else if (num_columns >= 4) {
		row = col = 2;
	} else if (num_columns == 3) {
		row = 3;
		col = 1;
	} else if (num_columns == 2) {
		row = 2;
		col = 1;
	}

	*bss << "set datafile separator \",\"" << std::endl
		 << "set format y '%g'" << std::endl
		 << "set key autotitle columnhead" << std::endl
		 << "set terminal png size " << w << "," << h << std::endl;

	*bss << "set output ";
	PutQuotedPath(output_path, bss);
	*bss << std::endl;

	*bss << "set multiplot layout " << row << "," << col << " rowsfirst downwards" << std::endl;
	unsigned int n = row*col+1;
	for (unsigned int i=2;i<=n;i++) {
		*bss << "plot ";
		PutQuotedPath(csv_path, bss);
		*bss << " using 1:" << i << " axes x1y1 with lines" << std::endl;
	}
	*bss << "unset multiplot" << std::endl;
}

#ifdef _WIN32

// Windows

template<typename TChar>
int CallGnuplot(const char *gnuplot,
				std::uint32_t num_columns,
				const TChar *csv_path,
				const char *output_path)
{
	SECURITY_ATTRIBUTES saAttr;
	saAttr.nLength = sizeof(SECURITY_ATTRIBUTES);
	saAttr.bInheritHandle = TRUE;
	saAttr.lpSecurityDescriptor = nullptr;

	HANDLE hChildInRead, hChildInWrite;
	if (!CreatePipe(&hChildInRead, &hChildInWrite, &saAttr, 0)) {
		std::cerr << "could not create pipe" << std::endl;
		return EXIT_FAILURE;
	}
	if (!SetHandleInformation(hChildInWrite, HANDLE_FLAG_INHERIT, 0)) {
		std::cerr << "could not set handle information" << std::endl;
		CloseHandle(hChildInWrite);
		CloseHandle(hChildInRead);
		return EXIT_FAILURE;
	}

	STARTUPINFO siStartInfo;
	GetStartupInfo(&siStartInfo);
	siStartInfo.hStdInput = hChildInRead;
	siStartInfo.hStdOutput = GetStdHandle(STD_OUTPUT_HANDLE);
	siStartInfo.hStdError = GetStdHandle(STD_ERROR_HANDLE);
	siStartInfo.dwFlags |= STARTF_USESTDHANDLES;
	PROCESS_INFORMATION piProcInfo;
	ZeroMemory(&piProcInfo, sizeof(piProcInfo));
	bool b = CreateProcess(gnuplot,
						   nullptr,
						   nullptr,
						   nullptr,
						   TRUE,
						   0,
						   nullptr,
						   nullptr,
						   &siStartInfo,
						   &piProcInfo);
	if (!b) {
		std::cerr << "could not create process: " << GetLastError() << std::endl;
		// should follow GetLastError()
		CloseHandle(hChildInWrite);
		CloseHandle(hChildInRead);
		return EXIT_FAILURE;
	}
	CloseHandle(hChildInRead); // close unused read end

	std::ostringstream bss;
	CreateScript(num_columns, csv_path, output_path, &bss);
	const std::string &buf(bss.str());
	WriteFile(hChildInWrite, (LPCVOID)buf.c_str(), (DWORD)buf.size(), nullptr, nullptr);
	CloseHandle(hChildInWrite);

	WaitForSingleObject(piProcInfo.hProcess, INFINITE);
	DWORD s;
	bool r = GetExitCodeProcess(piProcInfo.hProcess, &s);
	CloseHandle(piProcInfo.hThread);
	CloseHandle(piProcInfo.hProcess);
	if (r) {
		if (s == STILL_ACTIVE) {
			return EXIT_FAILURE;
		} else if (s == 0) {
			return EXIT_SUCCESS;
		} else { // TODO
			return EXIT_FAILURE;
		}
	}
	return EXIT_FAILURE;
}

#elif defined(HAVE_POPEN)

// POSIX

template<typename TChar>
int CallGnuplot(const char *gnuplot,
				std::uint32_t num_columns,
				const TChar *csv_path,
				const char *output_path)
{
	FILE *fp = popen(gnuplot, "w");
	if (!fp) {
		std::cerr << "failed to popen: "  << gnuplot << std::endl;
		return EXIT_FAILURE;
	}
	std::ostringstream bss;
	CreateScript(num_columns, csv_path, output_path, &bss);
	auto buf = bss.str();
	int result = EXIT_SUCCESS;
	if (std::fwrite(buf.c_str(), buf.size(), 1, fp) <= 0) {
		std::cerr << "failed to write script into pipe" << std::endl;
		result = EXIT_FAILURE;
	}
	int r = pclose(fp);
	if (r == -1) {
		std::cerr << "failed to pclose: " << std::strerror(errno) << std::endl;
		result = EXIT_FAILURE;
	}
	return result;
}

#else
#error "unavailability of popen is fatal."
#endif

} // namespace

int main(int argc, char *argv[])
{
	po::options_description opts("options");
	po::positional_options_description popts;
	po::variables_map vm;
	std::string gnuplot, input_file, output_file;
	int print_help = 0;

	opts.add_options()
		("gnuplot", boost::program_options::value<std::string>(&gnuplot)->default_value("gnuplot"),
		 "Command for gnuplot")
		("ignore-prefixes,P", "Ignore variable prefixes")
		("ignore-units,U", "Ignore units")
		("help,h", "Show this message")
		("output,o", po::value<std::string>(&output_file), "Output file name")
		("input", po::value<std::string>(&input_file), "Input file name");
	popts.add("input", 1);

	try {
		po::store(po::command_line_parser(argc, argv).options(opts).positional(popts).run(), vm);
		po::notify(vm);
		if (vm.count("help")) print_help = 1;
		if (vm.count("output") == 0) print_help = 2;
		if (vm.count("input") == 0) print_help = 2;
	} catch (const po::error &) {
		print_help = 2;
	}
	if (print_help != 0) {
		std::cerr << "usage: isdplot [OPTIONS] PATH" << std::endl;
		std::cerr << opts << std::endl;
		return (print_help == 1) ? EXIT_SUCCESS : EXIT_FAILURE;
	}

	isd2csv::Option isd2csv_option;
	isd2csv_option.ignore_prefixes = (vm.count("ignore-prefixes") > 0);
	isd2csv_option.ignore_units = (vm.count("ignore-units") > 0);

	std::vector<std::uint32_t> cv;
	if (!isdstrip::ExtractConstantColumns(input_file.c_str(), nullptr, &cv))
		return EXIT_FAILURE;
	boost::filesystem::path input_path(input_file.c_str());
	std::unique_ptr<TemporaryFile> temp_file;
	if (!cv.empty()) { // create a stripped file
		temp_file.reset(new TemporaryFile);
		if (!temp_file->ofs()) {
			std::cerr << "failed to open temporary file: " << temp_file->path() << std::endl;
			return EXIT_FAILURE;
		}
		int r = isdstrip::Filter(input_file.c_str(), cv, &temp_file->ofs());
		temp_file->Close();
		if (r != EXIT_SUCCESS) {
			std::cerr << "failed to filter constant columns: " << r << std::endl;
			return r;
		}
		input_path = temp_file->path();
	}

	std::uint32_t num_columns = 0;
	if (!CountColumns(input_path, &num_columns)) {
		return EXIT_FAILURE;
	}
	if (num_columns == 0) {
		std::cerr << "there seems no variable columns in: " << input_file.c_str() << std::endl;
		return EXIT_FAILURE;
	}
	TemporaryFile csv_file;
	if (!csv_file.ofs()) {
		std::cerr << "could not create temporary path: " << csv_file.path() << std::endl;
		return EXIT_FAILURE;
	}
	int r = CallIsd2csv(isd2csv_option, input_path, &csv_file.ofs());
	csv_file.Close();
	if (r != EXIT_SUCCESS)
		return r;
	r = CallGnuplot(gnuplot.c_str(), num_columns, csv_file.path().c_str(), output_file.c_str());
	return r;
}
