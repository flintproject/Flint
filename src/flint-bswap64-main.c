/* -*- Mode: C; tab-width: 4; indent-tabs-mode: t; c-basic-offset: 4 -*- vim:set ts=4 sw=4 sts=4 noet: */
#ifdef HAVE_CONFIG_H
#include "config.h"
#endif

#include <stdint.h>
#include <stdio.h>
#include <stdlib.h>

#define BSWAP_64(x)										\
	(((uint64_t)(x) << 56) |							\
	 (((uint64_t)(x) << 40) & 0xff000000000000ULL) |	\
	 (((uint64_t)(x) << 24) & 0xff0000000000ULL) |		\
	 (((uint64_t)(x) << 8)  & 0xff00000000ULL) |		\
	 (((uint64_t)(x) >> 8)  & 0xff000000ULL) |			\
	 (((uint64_t)(x) >> 24) & 0xff0000ULL) |			\
	 (((uint64_t)(x) >> 40) & 0xff00ULL) |				\
	 ((uint64_t)(x)  >> 56))

int main(int argc, char *argv[])
{
	FILE *ifp, *ofp;
	uint64_t x;
	int r = EXIT_SUCCESS;

	if (argc < 3) {
		fprintf(stderr, "usage: %s INPUT OUTPUT\n", argv[0]);
		return EXIT_FAILURE;
	}

	ifp = fopen(argv[1], "rb");
	if (!ifp) {
		fprintf(stderr, "could not open input file: %s\n", argv[1]);
		return EXIT_FAILURE;
	}

	ofp = fopen(argv[2], "wb");
	if (!ofp) {
		fclose(ifp);
		fprintf(stderr, "could not open output file: %s\n", argv[2]);
		return EXIT_FAILURE;
	}

	for (;;) {
		if (fread(&x, sizeof(x), 1, ifp) != 1) {
			if (ferror(ifp)) {
				fprintf(stderr, "failed to read input\n");
				r = EXIT_FAILURE;
			}
			goto bail;
		}
		x = BSWAP_64(x);
		if (fwrite(&x, sizeof(x), 1, ofp) != 1) {
			fprintf(stderr, "failed to write output\n");
			r = EXIT_FAILURE;
			goto bail;
		}
	}

 bail:
	fclose(ofp);
	fclose(ifp);

	return r;
}
