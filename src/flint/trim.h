/* -*- Mode: C++; tab-width: 4; indent-tabs-mode: t; c-basic-offset: 4 -*- vim:set ts=4 sw=4 sts=4 noet: */
#ifndef FLINT_TRIM_H_
#define FLINT_TRIM_H_

#include <libxml/xmlstring.h>

#ifdef __cplusplus
extern "C" {
#endif

/*
 * Drop spaces from both end of the UTF-8 string given as the first argument,
 * and store the result in the second argument.
 * Note that this function may modify the given string.
 * Return 1 in case of success, 0 otherwise.
 */
int Trim(xmlChar *s, xmlChar **tp);

#ifdef __cplusplus
}
#endif

#endif
