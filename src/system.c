/* -*- Mode: C; tab-width: 4; indent-tabs-mode: t; c-basic-offset: 4 -*- vim:set ts=4 sw=4 sts=4 noet: */
#include "system.h"

#include <assert.h>
#include <errno.h>
#include <stdio.h>
#include <stdlib.h>

#ifndef _WIN32
#include <sys/types.h>
#include <sys/wait.h>
#endif

int RunSystem(const char *command)
{
	assert(command);
#ifdef _WIN32
	/* We have to flush all output streams, as mentioned in
	   http://msdn.microsoft.com/en-us/library/277bwbdz.aspx
	*/
	(void)_flushall();
#endif
	int r = system(NULL);
	if (r == 0) {
		fprintf(stderr, "could not find a command interpreter\n");
		return -1;
	}
	r = system(command);
#ifdef _WIN32
	if (r == -1) {
		switch (errno) {
		case E2BIG:
			fprintf(stderr, "argument list is too big: %s\n", command);
			break;
		case ENOEXEC:
			fprintf(stderr, "invalid command format: %s\n", command);
			break;
		case ENOMEM:
			fprintf(stderr, "no enough memory to execute command: %s\n", command);
			break;
		default:
			fprintf(stderr, "unexpected error: %d: %s\n", errno, command);
			break;
		}
		return EXIT_FAILURE;
	}
	return r;
#else
	int status = EXIT_FAILURE;
	if (r == -1) {
		fprintf(stderr, "failed to call system: %s\n", command);
	} else if (WIFEXITED(r)) {
		status = WEXITSTATUS(r);
	}
	return status;
#endif
}
