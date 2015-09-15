/* -*- Mode: C; tab-width: 4; indent-tabs-mode: t; c-basic-offset: 4 -*- vim:set ts=4 sw=4 sts=4 noet: */
#ifndef FLINT_PROCESS_H_
#define FLINT_PROCESS_H_

#include <stdio.h>

#ifdef __cplusplus
extern "C" {
#endif

/*
 * Write the process id into the file.
 * Return a positive integer in case of success, 0 or a negative one otherwise.
 */
int WriteCurrentProcessId(FILE *fp);

#ifdef __cplusplus
}
#endif

#endif
