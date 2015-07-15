/* -*- Mode: C++; tab-width: 4; indent-tabs-mode: t; c-basic-offset: 4 -*- vim:set ts=4 sw=4 sts=4 noet: */
#ifndef FLINT_NUMERIC_PRNG_H_
#define FLINT_NUMERIC_PRNG_H_

#include <random>

template<typename TReal, typename TRng>
TReal GetExponentialVariate(TReal lmbd, TRng *rng)
{
	if (lmbd > 0) {
		std::exponential_distribution<TReal> p(lmbd);
		return p(*rng);
	}
	return 0.0; // FIXME
}

template<typename TReal, typename TRng>
int GetPoissonVariate(TReal mean, TRng *rng)
{
	if (mean > 0) {
		std::poisson_distribution<int> p(mean);
		return p(*rng);
	}
	return 0.0; // FIXME
}

template<typename TReal, typename TRng>
TReal GetGammaVariate(TReal alpha, TReal beta, TRng *rng)
{
	if (alpha > 0 && beta > 0) {
		std::gamma_distribution<TReal> g(alpha, beta);
		return g(*rng);
	}
	return 0.0; // FIXME
}

template<typename TReal, typename TRng>
TReal GetGaussVariate(TReal mean, TReal sigma, TRng *rng)
{
	std::normal_distribution<> norm(mean, sigma);
	return norm(*rng);
}

template<typename TReal, typename TRng>
TReal GetUniformVariate(TReal x, TReal y, TRng *rng)
{
	std::uniform_real_distribution<TReal> ur(x, y);
	return ur(*rng);
}

#endif // FLINT_NUMERIC_PRNG_H_
