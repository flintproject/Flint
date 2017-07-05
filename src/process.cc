/* -*- Mode: C++; tab-width: 4; indent-tabs-mode: t; c-basic-offset: 4 -*- vim:set ts=4 sw=4 sts=4 noet: */
#include "flint/process.h"

#ifdef _WIN32
#include <windows.h>
#else
#include <sys/types.h>
#include <unistd.h>
#endif

void WriteCurrentProcessId(std::ostream &ofs)
{
#ifdef _WIN32
	DWORD pid = GetCurrentProcessId();
#else
	pid_t pid = getpid();
#endif
	ofs << pid;
}
