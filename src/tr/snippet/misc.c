
static void fill_input(void)
{
	memcpy(data, input, sizeof(double)*layer_size);
	for (int i=1;i<nol;i++)
		memcpy(data+(layer_size*i), data, sizeof(double)*layer_size);
	for (int i=0;i<nol;i++)
		memcpy(prev+(layer_size*i), data, sizeof(double)*layer_size);
}

static size_t write_count = 0;

static int write_output(FILE *fp)
{
	if (write_count++%steps_per_output != 0)
		return 1;
	for (size_t i=0;i<(sizeof(output_range)/sizeof(output_range[0]));i++) {
		size_t s = fwrite(data+output_range[i].i, sizeof(double), output_range[i].s, fp);
		if (s != output_range[i].s) {
			fprintf(stderr, "failed to write output\n");
			return 0;
		}
	}
	return 1;
}

static void usage(const char *name)
{
	fprintf(stderr, "usage: %s output_file\n", name);
}
