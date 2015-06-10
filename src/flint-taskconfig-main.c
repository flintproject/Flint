/* -*- Mode: C++; tab-width: 4; indent-tabs-mode: t; c-basic-offset: 4 -*- vim:set ts=4 sw=4 sts=4 noet: */
#ifdef HAVE_CONFIG_H
#include "config.h"
#endif

#include <ctype.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

static void PrintRecipeForEuler(void)
{
	printf("bc: modeldb\n");
	printf("\tflint-compile modeldb input_eqs euler euler.db > $@\n");
	printf("\n");
}

static void PrintRecipeForRk4(void)
{
	printf("bc: modeldb\n");
	printf("\tflint-compile modeldb input_eqs rk4 rk4.db > $@\n");
	printf("\n");
}

static void Usage(void)
{
	fprintf(stderr, "usage: flint-taskconfig MODE CONF\n");
}

int main(int argc, char *argv[])
{
	if (argc == 2) {
		Usage();
		if ( strcmp(argv[1], "-h") == 0 ||
			 strcmp(argv[1], "--help") == 0 ) {
			return EXIT_SUCCESS;
		}
		return EXIT_FAILURE;
	}
	if (argc != 3) {
		Usage();
		return EXIT_FAILURE;
	}

	int is_run_mode = (strcmp(argv[1], "run") == 0);

	FILE *fp = fopen(argv[2], "r");
	if (!fp) {
		fprintf(stderr, "failed to open %s\n", argv[2]);
		return EXIT_FAILURE;
	}
	char buf[1024]; /* TODO: long enough? */
	size_t s = fread(buf, 1, 1023, fp);
	if (s < 1) {
		fprintf(stderr, "failed to read input\n");
		return EXIT_FAILURE;
	}
	fclose(fp);
	buf[s] = '\0';

	for (int i=0;i<1024 && buf[i];i++) {
		if (isspace(buf[i])) {
			buf[i] = '\0';
		}
	}
	char *type = buf;
	char *length = NULL;
	char *step = NULL;
	char *granularity = NULL;
	for (int i=0;i<1024;i++) {
		if (buf[i] == '\0') {
			do {i++;} while (buf[i] == '\0');
			if (!length) {
				length = buf+i;
			} else if (!step) {
				step = buf+i;
			} else if (!granularity) {
				granularity = buf+i;
				break;
			}
		}
	}
	if (!length || !step || !granularity) {
		fprintf(stderr, "invalid format: %s\n", argv[2]);
		return EXIT_FAILURE;
	}

	printf("include load.mk\n");
	printf("\n");
	if (strcmp(type, "euler") == 0) {
		PrintRecipeForEuler();
	} else {
		PrintRecipeForRk4();
	}

	printf("define generate\n");
	printf("$(1)/generated.db: db form.txt | $(1)\n");
	printf("\tflint-generate $$< $$@\n");
	printf("\n");
	printf("$(1)/generated-bc: $(1)/generated.db\n");
	printf("\tflint-compile $$< parameter_eqs assign $(1)/output.db > $$@\n");
	printf("\n");
	printf("$(1)/generated-init: db generated-layout $(1)/generated-bc\n");
	printf("\tflint-init $$^ $$@\n");
	printf("\n");
	printf("$(1)/stored: init generated-layout $(1)/generated-init layout\n");
	printf("\tflint-concat $$< $$@\n");
	printf("\tflint-store db generated-layout $(1)/generated-init layout $$@\n");
	printf("endef\n");
	printf("\n");

	printf("define simulate\n");
	/* Use init as the step 0 in case of the 'run' mode, otherwise
	   generate the step 0 from parameters
	*/
	if (is_run_mode) {
		printf("$(1)/start: init | $(1)\n");
	} else {
		printf("$(1)/start: $(1)/stored\n");
	}
	printf("\tflint-timer 0 %s %s $$< > $$@\n", length, step);
	printf("\n");
	printf("$(1)/first: filter $(1)/start\n");
	printf("\tflint-cut filter < $(1)/start > $$@\n");
	printf("\n");
	printf("$(1)/control: | $(1)\n");
	printf("\techo 0 > $$@\n"); /* TODO */
	printf("\n");
	printf("$(1)/run: isdh $(1)/first filter $(1)/start $(1)/control modeldb layout bc $(if $(filter phml,$(MODEL_LANG)),before-bc after-bc)\n");
	printf("\tflint-concat isdh $(1)/first $(1)/isd\n");
	printf("\tflint-evolve"
		   " --filter filter"
		   " --granularity %s"
		   " --input-data $(1)/start"
		   " --control $(1)/control"
		   " --output-data $(1)/output-data"
		   " --output-history $(1)/output-history"
		   " --status $(1)/status"
		   " $(if $(filter phml,$(MODEL_LANG)),--pre before-bc --post after-bc)"
		   " modeldb layout bc $(1)/isd\n",
		   granularity);
	printf("\tflint-concat $$@\n");
	printf("endef\n");
	printf("\n");

	printf("$(foreach job,$(JOBS),$(eval $(call generate,$(job))))\n");
	printf("$(foreach job,$(JOBS),$(eval $(call simulate,$(job))))\n");
	printf("\n");
	printf("$(JOBS):\n");
	printf("\tmkdir $@\n");
	printf("\n");

	if (is_run_mode) {
		printf("all: $(foreach job,$(JOBS),$(job)/run)\n");
	} else {
		printf("all: track $(foreach job,$(JOBS),$(job)/run)\n");
	}
	printf(".DEFAULT_GOAL = all\n");
	printf("\n");

	return EXIT_SUCCESS;
}
