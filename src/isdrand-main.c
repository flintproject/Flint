/* -*- Mode: C; tab-width: 4; indent-tabs-mode: t; c-basic-offset: 4 -*- vim:set ts=4 sw=4 sts=4 noet: */
#ifdef HAVE_CONFIG_H
#include "config.h"
#endif

#include <stdint.h>
#include <stdio.h>
#include <stdlib.h>
#include <time.h>

static const char LETTERS[] =
	"ABCDEFGHIJKLMNOPQRSTUVWXYZ"
	"abcdefghijklmnopqrstuvwxyz"
	"_0123456789";

static void generate_alpha(FILE *fp)
{
	fputc(LETTERS[rand()%52], fp);
}

static void generate_letter(FILE *fp)
{
	fputc(LETTERS[rand()%63], fp);
}

static void generate_name(int len, FILE *fp)
{
	generate_alpha(fp);
	for (int i=1;i<len;i++) generate_letter(fp);
}

static void generate_double(FILE *fp)
{
	double d;
	int k = rand();
	if (k == RAND_MAX) {
		d = atof((rand()%2) ? "inf" : "nan");
	} else {
		double f = ((double)k)/RAND_MAX; // fractional part
		int i = rand()%1024; // integer part
		d = i+f;
	}
	if (rand()%2) d = -d; // sign
	fwrite(&d, sizeof(double), 1, fp);
}

static const uint32_t MAX_NUM_OBJS = 100;
static const uint32_t MAX_NUM_BYTES_COMMENT = 4096;
static const uint32_t MAX_NAME_LEN = 32;
static const uint32_t MAX_NUM_STEPS = 1024;

int main(int argc, char *argv[])
{
	if (argc < 2) {
		fprintf(stderr, "usage: %s PATH [SEED]\n", argv[0]);
		return EXIT_FAILURE;
	}

	FILE *fp = fopen(argv[1], "wb");
	if (!fp) {
		perror(argv[1]);
		return EXIT_FAILURE;
	}

	time_t t = time(NULL);
	if (argc > 2) {
		int seed = atoi(argv[2]);
		srand((unsigned int)seed);
	} else {
		srand((unsigned int)t);
	}

	fprintf(fp, "ISDF"); // magic
	fputc(1, fp); // version

	char endian = 1; // endian: TODO
	fputc(endian, fp);

	// padding
	fputc(0, fp); fputc(0, fp);

	// timestamp
	struct tm *utc = gmtime(&t);
	char timestamp[21];
	// Note that on Windows strftime() comes from MSVCRT.LIB,
	// so some of the formatting codes are unavailable.
	if (strftime(timestamp, 21, "%Y-%m-%dT%H:%M:%SZ", utc) != 0) {
		fprintf(fp, "%s", timestamp);
	} else {
		fprintf(fp, "1970-01-01T00:00:00Z");
	}

	// num_objs
	uint32_t num_objs = rand()%MAX_NUM_OBJS;
	fwrite(&num_objs, sizeof(num_objs), 1, fp);

	// num_bytes_comment
	uint32_t num_bytes_comment = rand()%MAX_NUM_BYTES_COMMENT;
	fwrite(&num_bytes_comment, sizeof(num_bytes_comment), 1, fp);

	uint32_t *d = calloc(num_objs, sizeof(uint32_t));
	if (!d) {
		fprintf(stderr, "failed to calloc\n");
		fclose(fp);
		return EXIT_FAILURE;
	}

	// num_bytes_descs
	uint32_t num_bytes_descs = 0;
	for (uint32_t i=0;i<num_objs;i++) {
		uint32_t len = (rand()%MAX_NAME_LEN)+1;
		d[i] = len;
		num_bytes_descs += sizeof(uint32_t) + len;
	}
	fwrite(&num_bytes_descs, sizeof(num_bytes_descs), 1, fp);

	uint32_t *u = calloc(num_objs, sizeof(uint32_t));
	if (!u) {
		fprintf(stderr, "failed to calloc\n");
		free(d);
		fclose(fp);
		return EXIT_FAILURE;
	}

	// num_bytes_units
	uint32_t num_bytes_units = 0;
	for (uint32_t i=0;i<num_objs;i++) {
		uint32_t len = (rand()%MAX_NAME_LEN)+1;
		u[i] = len;
		num_bytes_units += sizeof(uint32_t) + len;
	}
	fwrite(&num_bytes_units, sizeof(num_bytes_units), 1, fp);

	// comment
	for (uint32_t i=0;i<num_bytes_comment;i++) {
		fputc(rand(), fp);
	}

	// descriptions
	for (uint32_t i=0;i<num_objs;i++) {
		fwrite(d+i, sizeof(d[i]), 1, fp);
		generate_name(d[i], fp);
	}
	free(d);

	// units
	for (uint32_t i=0;i<num_objs;i++) {
		fwrite(u+i, sizeof(u[i]), 1, fp);
		generate_name(u[i], fp);
	}
	free(u);

	// body
	uint32_t num_steps = rand()%MAX_NUM_STEPS;
	for (uint32_t i=0;i<num_steps;i++) {
		for (uint32_t k=0;k<num_objs;k++) {
			generate_double(fp);
		}
	}

	fclose(fp);

	return EXIT_SUCCESS;
}
