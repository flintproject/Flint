/* -*- Mode: C++; tab-width: 4; indent-tabs-mode: t; c-basic-offset: 4 -*- vim:set ts=4 sw=4 sts=4 noet: */
#ifndef FLINT_NUMERIC_PRNG_H_
#define FLINT_NUMERIC_PRNG_H_

#include <boost/random.hpp>
#include <boost/random/exponential_distribution.hpp>
#include <boost/random/gamma_distribution.hpp>
#include <boost/random/normal_distribution.hpp>
#include <boost/random/poisson_distribution.hpp>
#include <boost/random/uniform_real.hpp>

template<typename TReal, typename TRng>
TReal GetExponentialVariate(TReal lmbd, TRng *rng)
{
	if (lmbd > 0) {
		boost::exponential_distribution<TReal> p(lmbd);
		boost::variate_generator<TRng &, boost::exponential_distribution<TReal> > die(*rng, p);
		return die();
	}
	return 0.0; // FIXME
}

template<typename TReal, typename TRng>
TReal GetPoissonVariate(TReal mean, TRng *rng)
{
	if (mean > 0) {
		boost::poisson_distribution<int, TReal> p(mean);
		boost::variate_generator<TRng &, boost::poisson_distribution<int, TReal> > die(*rng, p);
		return die();
	}
	return 0.0; // FIXME
}

template<typename TReal, typename TRng>
TReal GetGammaVariate(TReal alpha, TReal beta, TRng *rng)
{
	if (alpha > 0 && beta > 0) {
		boost::gamma_distribution<TReal> g(alpha, 1/beta);
		boost::variate_generator<TRng &, boost::gamma_distribution<TReal> > die(*rng, g);
		return die();
	}
	return 0.0; // FIXME
}

template<typename TReal, typename TRng>
TReal GetGaussVariate(TReal mean, TReal sigma, TRng *rng)
{
	boost::normal_distribution<> norm(mean, sigma);
	boost::variate_generator<TRng &, boost::normal_distribution<> > die(*rng, norm);
	return die();
}

template<typename TReal, typename TRng>
TReal GetUniformVariate(TReal x, TReal y, TRng *rng)
{
	boost::uniform_real<TReal> norm(x, y);
	boost::variate_generator<TRng &, boost::uniform_real<TReal> > die(*rng, norm);
	return die();
}

#endif // FLINT_NUMERIC_PRNG_H_
