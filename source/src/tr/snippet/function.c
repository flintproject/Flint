
double modulo(double d1, double d2)
{
	double r_t = fmod(d1, d2);
	if (r_t < 0)
		r_t += fabs(d2);
	return r_t;
}
