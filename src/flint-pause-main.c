/* -*- Mode: C; tab-width: 4; indent-tabs-mode: t; c-basic-offset: 4 -*- vim:set ts=4 sw=4 sts=4 noet: */
#include <errno.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#ifdef _WIN32
#include <windows.h>
#include <tlhelp32.h>
#else
#include <signal.h>
#include <sys/types.h>
#endif

static int Notify(int pid)
{
#ifdef _WIN32
	int r = 0;
	HANDLE hSnapshot = CreateToolhelp32Snapshot(TH32CS_SNAPTHREAD, 0);
	if (hSnapshot == INVALID_HANDLE_VALUE) {
		fprintf(stderr, "failed to create a snapshot: %ld\n", GetLastError());
		return 0;
	}
	THREADENTRY32 te;
	te.dwSize = sizeof(te);
	if (Thread32First(hSnapshot, &te)) {
		do {
			if (te.dwSize == FIELD_OFFSET(THREADENTRY32, th32OwnerProcessID) + sizeof(te.th32OwnerProcessID) &&
				te.th32OwnerProcessID == (DWORD)pid) {
				HANDLE hThread = OpenThread(THREAD_SUSPEND_RESUME, FALSE, te.th32ThreadID);
				if (!hThread) {
					fprintf(stderr, "failed to open a thread\n");
					goto bail;
				}
				DWORD dwResult = SuspendThread(hThread);
				CloseHandle(hThread);
				if (dwResult == (DWORD)-1) {
					fprintf(stderr, "failed to suspend a thread\n");
					goto bail;
				}
			}
		} while (Thread32Next(hSnapshot, &te));
	}
	r = 1;

 bail:
	CloseHandle(hSnapshot);
	return r;
#else
	return kill((pid_t)pid, SIGSTOP) == 0;
#endif
}

static void Usage(void)
{
	fprintf(stderr, "usage: flint-pause pid\n");
}

int main(int argc, char *argv[])
{
	if (argc != 2) {
		Usage();
		return EXIT_FAILURE;
	}
	if ( strcmp(argv[1], "-h") == 0 ||
		 strcmp(argv[1], "--help") == 0 ) {
		Usage();
		return EXIT_SUCCESS;
	}
	int pid = atoi(argv[1]);
	if (pid <= 0) {
		fprintf(stderr, "invalid pid: %s\n", argv[1]);
		return EXIT_FAILURE;
	}
	if (!Notify(pid)) {
		perror(argv[1]);
		return EXIT_FAILURE;
	}
	return EXIT_SUCCESS;
}
