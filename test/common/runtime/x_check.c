#include <errno.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

int main(int argc, char *argv[])
{
	char buf[56];
	double expected, a, b, x;
	size_t s;
	FILE *fp;
	int r;

	if (argc < 3) return EXIT_FAILURE;
	expected = atof(argv[1]);
	fp = fopen(argv[2], "rb");
	if (!fp) {
		fprintf(stderr, "%s\n", strerror(errno));
		return EXIT_FAILURE;
	}

	s = fread(buf, 56, 1, fp);
	if (!s) {
		r = EXIT_FAILURE;
		goto bail;
	}
	memcpy(&x, &buf[48], sizeof(x));
	if (x == expected) {
		r = EXIT_SUCCESS;
		goto bail;
	}

	memcpy(&a, &buf[32], sizeof(a));
	memcpy(&b, &buf[40], sizeof(b));
	fprintf(stderr, "%s is expected, but got %f: %g|%g|%g\n", argv[1], x, a, b, x);
	r = EXIT_FAILURE;

 bail:
	fclose(fp);
	return r;
}
