/* -*- Mode: C; tab-width: 4; indent-tabs-mode: t; c-basic-offset: 4 -*- vim:set ts=4 sw=4 sts=4 noet: */
#ifdef HAVE_CONFIG_H
#include "config.h"
#endif

#include <stdint.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#if defined(_WIN32) && defined(__MINGW32__)
#define le32toh(x) (x)
#define be32toh(x) BSWAP_32(x)

#elif defined(__MACH__) && defined(__APPLE__)
#ifdef __LITTLE_ENDIAN__
#include <arpa/inet.h>
#define le32toh(x) (x)
#define be32toh(x) ntohl(x)
#else /* big endian */
#define le32toh(x) BSWAP_32(x)
#define be32toh(x) (x)
#endif

#elif defined(__linux__)
#include <endian.h>

/* for K's ppb */
#ifdef __GNUC__
#if __GNUC__ == 4 && __GNUC_MINOR__ == 1
#include <byteswap.h>
#if __BYTE_ORDER == __LITTLE_ENDIAN
#define le32toh(x) (x)
#define be32toh(x) bswap_32(x)
#else /* big endian */
#define le32toh(x) bswap_32(x)
#define be32toh(x) (x)
#endif
#endif
#endif

#else
#error "could not determine endianness"
#endif

#define BSWAP_32(x)								\
	(((uint32_t)(x) << 24) |					\
	 (((uint32_t)(x) << 8) & 0x00ff0000UL) |	\
	 (((uint32_t)(x) >> 8) & 0x0000ff00UL) |	\
	 ((uint32_t)(x) >> 24))

#define BSWAP_64(x)										\
	(((uint64_t)(x) << 56) |							\
	 (((uint64_t)(x) << 40) & 0xff000000000000ULL) |	\
	 (((uint64_t)(x) << 24) & 0xff0000000000ULL) |		\
	 (((uint64_t)(x) << 8)  & 0xff00000000ULL) |		\
	 (((uint64_t)(x) >> 8)  & 0xff000000ULL) |			\
	 (((uint64_t)(x) >> 24) & 0xff0000ULL) |			\
	 (((uint64_t)(x) >> 40) & 0xff00ULL) |				\
	 ((uint64_t)(x)  >> 56))

static int read_header(FILE *ifp, FILE *ofp,
					   char *little_endian,
					   uint32_t *num_objs,
					   uint32_t *num_bytes_comment,
					   uint32_t *num_bytes_descs,
					   uint32_t *num_bytes_units)
{
	char header[44];
	size_t s;
	uint32_t no, hbc, hbd, hbu;
	char le;

	s = fread(header, sizeof(header), 1, ifp);
	if (!s) {
		fprintf(stderr, "failed to read header\n");
		return 0;
	}
	if ( header[0] != 'I' || header[1] != 'S' ||
		 header[2] != 'D' || header[3] != 'F' ) {
		fprintf(stderr, "invalid FourCC\n");
		return 0;
	}
	*little_endian = le = header[5];
	memcpy(&no, &header[28], 4);
	memcpy(&hbc, &header[32], 4);
	memcpy(&hbd, &header[36], 4);
	memcpy(&hbu, &header[40], 4);
	if (le) {
		*num_objs = le32toh(no);
		*num_bytes_comment = le32toh(hbc);
		*num_bytes_descs = le32toh(hbd);
		*num_bytes_units = le32toh(hbu);
	} else {
		*num_objs = be32toh(no);
		*num_bytes_comment = be32toh(hbc);
		*num_bytes_descs = be32toh(hbd);
		*num_bytes_units = be32toh(hbu);
	}

	if (le) {
		header[5] = 0;
	} else {
		header[5] = 1;
	}
	no = BSWAP_32(no);
	hbc = BSWAP_32(hbc);
	hbd = BSWAP_32(hbd);
	hbu = BSWAP_32(hbu);
	memcpy(&header[28], &no, 4);
	memcpy(&header[32], &hbc, 4);
	memcpy(&header[36], &hbd, 4);
	memcpy(&header[40], &hbu, 4);
	s = fwrite(header, sizeof(header), 1, ofp);
	if (!s) {
		fprintf(stderr, "failed to write header\n");
		return 0;
	}
	return 1;
}

static int copy_comment(FILE *ifp, FILE *ofp,
						uint32_t num_bytes_comment)
{
	char *buf = malloc(num_bytes_comment);
	if (!buf) {
		fprintf(stderr, "malloc failed\n");
		return 0;
	}
	if (!fread(buf, num_bytes_comment, 1, ifp)) {
		fprintf(stderr, "failed to read comment\n");
		free(buf);
		return 0;
	}
	if (!fwrite(buf, num_bytes_comment, 1, ofp)) {
		fprintf(stderr, "failed to write comment\n");
		free(buf);
		return 0;
	}
	free(buf);
	return 1;
}

static int read_descriptions(FILE *ifp, FILE *ofp,
							 char little_endian,
							 uint32_t num_objs,
							 uint32_t num_bytes_descs)
{
	char *buf;
	uint32_t i, len, hlen;
	size_t s;
	int r = 0;

	buf = malloc(num_bytes_descs);
	if (!buf) {
		fprintf(stderr, "malloc failed\n");
		return 0;
	}

	s = fread(buf, num_bytes_descs, 1, ifp);
	if (!s) {
		fprintf(stderr, "failed to read descriptions\n");
		goto bail;
	}

	s = 0;
	for (i = 0; i < num_objs; i++) {
		memcpy(&len, &buf[s], 4);
		hlen = little_endian ? le32toh(len) : be32toh(len);

		len = BSWAP_32(len);
		memcpy(&buf[s], &len, 4);

		s += (size_t)(hlen + 4);
	}
	if (s != (size_t)num_bytes_descs) {
		fprintf(stderr, "invalid descriptions\n");
		goto bail;
	}
	s = fwrite(buf, num_bytes_descs, 1, ofp);
	if (!s) {
		fprintf(stderr, "failed to write descriptions\n");
		goto bail;
	}
	r = 1;

 bail:
	free(buf);

	return r;
}

static int read_units(FILE *ifp, FILE *ofp,
					  char little_endian,
					  uint32_t num_objs,
					  uint32_t num_bytes_units)
{
	char *buf;
	uint32_t i, len, hlen;
	size_t s;
	int r = 0;

	buf = malloc(num_bytes_units);
	if (!buf) {
		fprintf(stderr, "malloc failed\n");
		return 0;
	}

	s = fread(buf, num_bytes_units, 1, ifp);
	if (!s) {
		fprintf(stderr, "failed to read units\n");
		goto bail;
	}

	s = 0;
	for (i = 0; i < num_objs; i++) {
		memcpy(&len, &buf[s], 4);
		hlen = little_endian ? le32toh(len) : be32toh(len);

		len = BSWAP_32(len);
		memcpy(&buf[s], &len, 4);

		s += (size_t)(hlen + 4);
	}
	if (s != (size_t)num_bytes_units) {
		fprintf(stderr, "invalid units\n");
		goto bail;
	}
	s = fwrite(buf, num_bytes_units, 1, ofp);
	if (!s) {
		fprintf(stderr, "failed to write units\n");
		goto bail;
	}
	r = 1;

 bail:
	free(buf);

	return r;
}

static int read_data(FILE *ifp, FILE *ofp)
{
	uint64_t x;

	for (;;) {
		if (fread(&x, sizeof(x), 1, ifp) != 1) {
			if (ferror(ifp)) {
				fprintf(stderr, "failed to read input\n");
				return 0;
			}
			break;
		}
		x = BSWAP_64(x);
		if (fwrite(&x, sizeof(x), 1, ofp) != 1) {
			fprintf(stderr, "failed to write output\n");
			return 0;
		}
	}
	return 1;
}

int main(int argc, char *argv[])
{
	FILE *ifp, *ofp;
	char little_endian;
	uint32_t num_objs, num_bytes_comment, num_bytes_descs, num_bytes_units;
	int r = EXIT_FAILURE;

	if (argc < 3) {
		fprintf(stderr, "usage: %s INPUT OUTPUT\n", argv[0]);
		return r;
	}

	ifp = fopen(argv[1], "rb");
	if (!ifp) {
		fprintf(stderr, "could not open input file: %s\n", argv[1]);
		return r;
	}

	ofp = fopen(argv[2], "wb");
	if (!ofp) {
		fclose(ifp);
		fprintf(stderr, "could not open output file: %s\n", argv[2]);
		return r;
	}

	if (!read_header(ifp, ofp,
					 &little_endian, &num_objs,
					 &num_bytes_comment, &num_bytes_descs, &num_bytes_units)) {
		goto bail;
	}

	if (!copy_comment(ifp, ofp, num_bytes_comment)) {
		goto bail;
	}

	if (!read_descriptions(ifp, ofp, little_endian, num_objs, num_bytes_descs)) {
		goto bail;
	}

	if (!read_units(ifp, ofp, little_endian, num_objs, num_bytes_units)) {
		goto bail;
	}

	if (!read_data(ifp, ofp)) {
		goto bail;
	}

	r = EXIT_SUCCESS;

 bail:
	fclose(ofp);
	fclose(ifp);

	return r;
}
