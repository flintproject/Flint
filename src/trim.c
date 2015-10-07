/* -*- Mode: C; tab-width: 4; indent-tabs-mode: t; c-basic-offset: 4 -*- vim:set ts=4 sw=4 sts=4 noet: */
#include "flint/trim.h"

#include <ctype.h>
#include <stdio.h>
#include <string.h>

int Trim(xmlChar *s, xmlChar **tp)
{
	int len = xmlStrlen(s);
	if (len == 0)
		goto success;

	do {
		int k = len;
		int c = xmlGetUTF8Char(s, &k);
		if (c < 0)
			goto error;
		if (!isspace(c))
			break;
		s += k;
		len -= k;
	} while (*s);

	int ulen = xmlUTF8Strlen(s);
	if (ulen < 0)
		goto error;
	if (ulen == 0)
		goto success;

	int pos = ulen-1;
	xmlChar *e = s;
	while (*e++) ; // go to the end
	for (;;) {
		xmlChar *p = xmlUTF8Strpos(s, pos);
		if (p == NULL)
			goto error;
		int k = e - p;
		int c = xmlGetUTF8Char(p, &k);
		if (c < 0)
			goto error;
		if (!isspace(c))
			goto success;
		*p = '\0';
		e = p;
		--pos;
	}

 success:
	*tp = s;
	return 1;

 error:
	fprintf(stderr, "invalid UTF-8 string: %s\n", (const char *)s);
	return 0;
}
