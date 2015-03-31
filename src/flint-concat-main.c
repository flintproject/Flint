/* -*- Mode: C++; tab-width: 4; indent-tabs-mode: t; c-basic-offset: 4 -*- vim:set ts=4 sw=4 sts=4 noet: */
#ifdef HAVE_CONFIG_H
#include "config.h"
#endif

#include <assert.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

static const size_t kBufferSize = 4096;

static void Usage(void)
{
	fprintf(stderr, "usage: flint-concat INPUT ... OUTPUT\n");
}

int main(int argc, char *argv[])
{
	if (argc == 1) {
		Usage();
		return EXIT_FAILURE;
	}
	if ( argc == 2 &&
		 ( strcmp("--help", argv[1]) == 0 ||
		   strcmp("-h", argv[1]) == 0 ) ) {
		return EXIT_SUCCESS;
	}

	char *buf = malloc(kBufferSize);
	if (!buf) {
		fprintf(stderr, "failed to malloc buffer\n");
		return EXIT_FAILURE;
	}

	int r = EXIT_FAILURE;
	const char *output = argv[argc-1];
	FILE *ofp = fopen(output, "wb");
	if (!ofp) {
		perror(output);
		goto bail0;
	}

	int i;
	for (i=1;i<argc-1;i++) {
		const char *input = argv[i];
		FILE *ifp = fopen(input, "rb");
		if (!ifp) {
			perror(input);
			goto bail1;
		}
		size_t s;
		while ( (s = fread(buf, 1, kBufferSize, ifp)) > 0) {
			size_t t = fwrite(buf, 1, s, ofp);
			if (t != s) {
				fprintf(stderr, "failed to write into %s\n", output);
				fclose(ifp);
				goto bail1;
			}
		}
		if (!feof(ifp)) {
			fprintf(stderr, "failed to read from %s\n", input);
			fclose(ifp);
			goto bail1;
		}
		fclose(ifp);
	}
	r = EXIT_SUCCESS;

 bail1:
	fclose(ofp);
 bail0:
	free(buf);
	return r;
}
