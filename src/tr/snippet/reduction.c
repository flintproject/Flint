
struct Reduction {
	void (*reduce)(const struct Reduction *r);
	int s;    /* size */
	int t;    /* target address */
	int nsa;  /* number of source addresses */
	int sa[]; /* source addresses */
};

void reduce_sum(const struct Reduction *r)
{
	for (int i=0;i<r->s;i++) {
		double d = 0;
		for (int k=0;k<r->nsa;k++) {
			d += data[r->sa[k]+i];
		}
		data[r->t+i] = d;
	}
}

void reduce_max(const struct Reduction *r)
{
	for (int i=0;i<r->s;i++) {
		double d = data[r->sa[0]+i];
		for (int k=1;k<r->nsa;k++) {
			double dd = data[r->sa[k]+i];
			if (d < dd)
				 d = dd;
		}
		data[r->t+i] = d;
	}
}

void reduce_min(const struct Reduction *r)
{
	for (int i=0;i<r->s;i++) {
		double d = data[r->sa[0]+i];
		for (int k=1;k<r->nsa;k++) {
			double dd = data[r->sa[k]+i];
			if (dd < d)
				 d = dd;
		}
		data[r->t+i] = d;
	}
}

void reduce_mean(const struct Reduction *r)
{
	for (int i=0;i<r->s;i++) {
		double d = 0;
		for (int k=0;k<r->nsa;k++) {
			d += data[r->sa[k]+i] / r->nsa;
		}
		data[r->t+i] = d;
	}
}

void reduce_degree(const struct Reduction *r)
{
	for (int i=0;i<r->s;i++)
		data[r->t+i] = r->nsa;
}
