/* -*- Mode: C; tab-width: 4; indent-tabs-mode: t; c-basic-offset: 4 -*- vim:set ts=4 sw=4 sts=4 noet: */
#ifndef FLINT_SYSTEM_H_
#define FLINT_SYSTEM_H_

#ifdef __cplusplus
extern "C" {
#endif

/*
 * Use this function instead of system().
 * Return 0 in case of success, non-zero otherwise.
 */
int RunSystem(const char *command);

#ifdef __cplusplus
}
#endif

#endif
