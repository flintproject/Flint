/* -*- Mode: C++; tab-width: 4; indent-tabs-mode: t; c-basic-offset: 4 -*- vim:set ts=4 sw=4 sts=4 noet: */
#ifndef FLINT_NUMERIC_PRNG_H_
#define FLINT_NUMERIC_PRNG_H_

#include <limits>
#include <random>

template<typename TReal>
TReal FallbackValue()
{
	if (std::numeric_limits<TReal>::has_signaling_NaN)
		return std::numeric_limits<TReal>::signaling_NaN();
	if (std::numeric_limits<TReal>::has_quiet_NaN)
		return std::numeric_limits<TReal>::quiet_NaN();
	return 0.0; // FIXME
}

template<typename TReal, typename TRng>
TReal GetExponentialVariate(TReal lmbd, TRng *rng)
{
	if (lmbd > 0) {
		std::exponential_distribution<TReal> p(lmbd);
		return p(*rng);
	}
	return FallbackValue<TReal>();
}

template<typename TReal, typename TRng>
int GetPoissonVariate(TReal mean, TRng *rng)
{
	if (mean > 0) {
		std::poisson_distribution<int> p(mean);
		return p(*rng);
	}
	return FallbackValue<TReal>();
}

template<typename TReal, typename TRng>
TReal GetGammaVariate(TReal alpha, TReal beta, TRng *rng)
{
	if (alpha > 0 && beta > 0) {
		std::gamma_distribution<TReal> g(alpha, beta);
		return g(*rng);
	}
	return FallbackValue<TReal>();
}

template<typename TReal, typename TRng>
TReal GetGaussVariate(TReal mean, TReal sigma, TRng *rng)
{
	std::normal_distribution<> norm(mean, sigma);
	return norm(*rng);
}

template<typename TReal, typename TRng>
TReal GetLognormalVariate(TReal mu, TReal sigma, TRng *rng)
{
	if (sigma > 0) {
		std::lognormal_distribution<> ln(mu, sigma);
		return ln(*rng);
	}
	return FallbackValue<TReal>();
}

template<typename TReal, typename TRng>
TReal GetUniformVariate(TReal x, TReal y, TRng *rng)
{
	std::uniform_real_distribution<TReal> ur(x, y);
	return ur(*rng);
}

template<typename TReal, typename TRng>
TReal GetWeibullVariate(TReal scale, TReal shape, TRng *rng)
{
	if (scale > 0 && shape > 0) {
		std::weibull_distribution<TReal> w(shape, scale); // parameters given in the reverse order
		return w(*rng);
	}
	return FallbackValue<TReal>();
}

#endif // FLINT_NUMERIC_PRNG_H_
