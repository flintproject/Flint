/* -*- Mode: C; tab-width: 4; indent-tabs-mode: t; c-basic-offset: 4 -*- vim:set ts=4 sw=4 sts=4 noet: */
#include "flint/process.h"

#ifdef _WIN32
#include <windows.h>
#else
#include <sys/types.h>
#include <unistd.h>
#endif

int WriteCurrentProcessId(FILE *fp)
{
#ifdef _WIN32
	DWORD pid = GetCurrentProcessId();
	return fprintf(fp, "%ld", pid);
#else
	pid_t pid = getpid();
	return fprintf(fp, "%d", (int)pid);
#endif
}
