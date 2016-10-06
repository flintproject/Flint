/* -*- Mode: C++; tab-width: 4; indent-tabs-mode: t; c-basic-offset: 4 -*- vim:set ts=4 sw=4 sts=4 noet: */
#ifdef HAVE_CONFIG_H
#include "config.h"
#endif

#include <cstdlib>
#include <cstring>
#include <iostream>
#include <iomanip>
#include <fstream>
#include <memory>
#include <string>
#include <sstream>

#ifdef HAVE_FORK
#include <errno.h>
#include <sys/wait.h>
#include <unistd.h>
#else
#include <process.h>
#include <windows.h>
#endif

#include <boost/program_options.hpp>
#include "isdf/isdf.h"
#include "sys/temporary_path.h"

#include "system.h"

namespace po = boost::program_options;

using std::ifstream;
using std::istream;
using std::ios;
using std::ostringstream;
using std::string;

using namespace flint;

namespace {

bool ReadHeader(istream *is, char *header)
{
	is->read(header, sizeof(isdf::ISDFHeader));
	return is->good();
}

bool ReadDescriptions(std::uint32_t num_bytes_descs, istream *is, char *rest)
{
	is->read(rest, num_bytes_descs);
	return is->good();
}

bool CountColumns(const char *input, std::uint32_t *num_columns)
{
	ifstream ifs(input, ios::in|ios::binary);
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

int CallIsdstrip(const string &isdstrip, const char *input, const char *output)
{
	static const char kCommand[] = "%s -o \"%s\" \"%s\"";

	size_t cmd_len = isdstrip.size() + strlen(input) + strlen(output) + 64;
	std::unique_ptr<char[]> cmd(new char[cmd_len]);
	sprintf(cmd.get(), kCommand, isdstrip.c_str(), output, input);
	int r = RunSystem(cmd.get());
	return r;
}

int CallIsd2csv(const string &isd2csv, const char *input, const char *output)
{
	static const char kCommand[] = "%s -o \"%s\" \"%s\"";

	size_t cmd_len = isd2csv.size() + strlen(input) + strlen(output) + 64;
	std::unique_ptr<char[]> cmd(new char[cmd_len]);
	sprintf(cmd.get(), kCommand, isd2csv.c_str(), output, input);
	int r = RunSystem(cmd.get());
	return r;
}

void PutQuotedPath(const char *path, ostringstream *bss)
{
	*bss << "\"";
	char c;
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

void CreateScript(std::uint32_t num_columns,
				  const char *csv_path,
				  const char *output_path,
				  ostringstream *bss)
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

#ifdef HAVE_FORK

// POSIX

int CallGnuplot(const char *gnuplot,
				std::uint32_t num_columns,
				const char *csv_path,
				const char *output_path)
{
	// prepare arguments of execv() before fork
	std::unique_ptr<char[]> arg0(new char[std::strlen(gnuplot)+1]);
	std::strcpy(arg0.get(), gnuplot);
	char *args[2];
	args[0] = arg0.get();
	args[1] = nullptr;

	int pipefd[2];
	if (pipe(pipefd) == -1) {
		std::cerr << "could not create pipe; " << strerror(errno);
		return EXIT_FAILURE;
	}
	pid_t pid = fork();
	if (pid == -1) { // fork failed
		close(pipefd[1]);
		close(pipefd[0]);
		std::cerr << "could not fork; " << strerror(errno);
		return EXIT_FAILURE;
	}
	if (pid == 0) { // child
		close(pipefd[1]); // close unused write end
		int r = dup2(pipefd[0], STDIN_FILENO); // replace starndard input
		if (r == -1) _Exit(EXIT_FAILURE);
		close(pipefd[0]);
		execv(args[0], args); // should not return
		_Exit(EXIT_FAILURE);
	}
	// parent
	close(pipefd[0]); // close unused read end
	int fd = pipefd[1];
	ostringstream bss;
	CreateScript(num_columns, csv_path, output_path, &bss);
	const string &buf(bss.str());
	if (write(fd, buf.c_str(), buf.size()) < 0) {
		std::cerr << "could not write into pipe; " << strerror(errno);
		close(fd);
		waitpid(pid, nullptr, 0);
		return EXIT_FAILURE;
	}
	close(fd);

	int s;
	waitpid(pid, &s, 0);
	if (WIFEXITED(s) && WEXITSTATUS(s) == 0) return EXIT_SUCCESS;
	return EXIT_FAILURE;
}

#else

// Windows

int CallGnuplot(const char *gnuplot,
				std::uint32_t num_columns,
				const char *csv_path,
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

	ostringstream bss;
	CreateScript(num_columns, csv_path, output_path, &bss);
	const string &buf(bss.str());
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

#endif

char *stripped_path = nullptr;
char *csv_path = nullptr;

void Cleanup(void)
{
	if (stripped_path) remove(stripped_path);
	if (csv_path) remove(csv_path);
}

} // namespace

int main(int argc, char *argv[])
{
	po::options_description opts("options");
	po::positional_options_description popts;
	po::variables_map vm;
	string gnuplot, isd2csv, isdstrip, input_file, output_file;
	int print_help = 0;

	opts.add_options()
		("gnuplot", boost::program_options::value<string>(&gnuplot),
		 "Command for gnuplot")
		("isd2csv", boost::program_options::value<string>(&isd2csv),
		 "Command for isd2csv")
		("isdstrip", boost::program_options::value<string>(&isdstrip),
		 "Command for isdstrip")
		("help,h", "Show this message")
		("output,o", po::value<string>(&output_file), "Output file name")
		("input", po::value<string>(&input_file), "Input file name");
	popts.add("input", 1);

	try {
		po::store(po::command_line_parser(argc, argv).options(opts).positional(popts).run(), vm);
		po::notify(vm);
		if (vm.count("help")) print_help = 1;
		if (vm.count("gnuplot") == 0) print_help = 2;
		if (vm.count("isd2csv") == 0) print_help = 2;
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

	std::atexit(Cleanup);

	std::unique_ptr<TemporaryPath> temp_path(new TemporaryPath("isdplot"));
	const char *input_path = input_file.c_str();

	if (vm.count("isdstrip")) {
		stripped_path = temp_path->Touch();
		if (!stripped_path) {
			std::cerr << "could not create temporary path" << std::endl;
			return EXIT_FAILURE;
		}
		int r = CallIsdstrip(isdstrip, input_path, stripped_path);
		if (r != EXIT_SUCCESS) {
			std::cerr << "isdstrip exit abnormally: " << r << std::endl;
			remove(stripped_path);
			free(stripped_path);
			return r;
		}
		input_path = stripped_path;
	}

	std::uint32_t num_columns = 0;
	if (!CountColumns(input_path, &num_columns)) {
		return EXIT_FAILURE;
	}
	csv_path = temp_path->Touch();
	if (!csv_path) {
		std::cerr << "could not create temporary path" << std::endl;
		return EXIT_FAILURE;
	}
	int r = CallIsd2csv(isd2csv, input_path, csv_path);
	if (r != EXIT_SUCCESS) {
		std::cerr << "isd2csv exit abnormally: " << r << std::endl;
		return r;
	}
	r = CallGnuplot(gnuplot.c_str(), num_columns, csv_path, output_file.c_str());
	return r;
}
