---
title: 'Flint: a simulator for biological and physiological models in ordinary and stochastic differential equations'
tags:
  - biology
  - physiology
  - numerical analysis
  - simulation
  - ODE
  - SDE
authors:
  - name: Takeshi Abe
    orcid: 0000-0002-7074-4561
    affiliation: 1
  - name: Yoshiyuki Asai
    orcid: 0000-0001-5519-4306
    affiliation: 1
affiliations:
 - name: Graduate School of Medicine, Yamaguchi University
   index: 1
date: 17 Aug 2020
bibliography: paper.bib
---

# Introduction

Understanding dynamics of living organisms often requires a mathematical model
that describes the hypotheses to be tested. It is widely recognized that the
class of ordinary differential equations (ODE) is suitable for describing the
time course of variables in a deterministic system, stemming from a simple
assumption about the rate of their change.
One of such examples is the chemical reaction accelerated by an enzyme
following the Michaelis-Menten kinetics; another is the action potential of
cardiac cells driven by modulation of ion channels. By a virtue of
differential equations, these celullar models can be integrated into the one of
tissue or organ level. In fact, ways to integrate a computational model of
the physiological functions of the whole individual have been explored since the
end of the last century, under the name physiome [@leem_perspectives_2016].

It is, however, technically challenging for practitioners in the field of
biology or physiology to express their hypotheses on biological organisms in a
precise system of ODEs. In order to make it easier to edit a model in a problem
that implicitly specify the ODEs, several domain-specific languages have
been proposed and standardized, including CellML [@lloyd_cellml_2004], the
Physiological Hierarchy Markup Language (PHML) devised by Asai and colleagues
[@asai_databases_2015], and the Systems Biology Markup Language (SBML) devised
by Hucka and colleagues [@hucka_systems_2003]. Although the design principles of each modeling
language vary, computational analysis of any model in these languages
comprises a shared set of procedures based on the theory of differential
equations and dynamical systems.

In this work we introduce `Flint`, a simulator software for models written in
the above languages. The simulator allows users to transform a given model into
a system of ODEs and solve it in a numerical manner. It also supports stochastic
differential equations (SDE), a non-deterministic extension of ODEs, which makes
it possible to involve random elements, e.g. noise, in the dynamics.

The development of `Flint` has been tied in with the physiome.jp project
[@nomura_toward_2010], which aims to establish a computational platform for
multiscale _in silico_ studies on the physiome. As part of the platform, `Flint`
complements the features of an authoring software PhysioDesigner for PHML
[@asai_multilevel_2012], while they are deliberately separate programs. Driven
by demands from the project's collaborators, we have enhanced `Flint` to support
different modeling standards. For example, in order to leverage a published SBML
model of subcellular signaling to build tissue or higher-level physiological
ones, there is a technical proposal embedding it in PHML
[@asai_versatile_2014]. Simulating such models is a reason for adopting `Flint`
even when other state-of-the-art tools are publicly available, e.g. COPASI
[@hoops_copasicomplex_2006] that focuses on its own format. `Flint`'s main
contribution is to provide an open, language-agnostic resource for reproducible
simulation studies.

# Implementation

## User interface

`Flint` is a standalone program that runs on consumer desktop environments such
as Microsoft Windows, Apple's macOS, and Linux with GTK. For the simplest usage,
its graphical user interface runs a simulation of a given model with only two
steps; open the model file, and select the Run button. Running simulations at the
command line is also supported, although only a limited number of the functions
are available in the command line interface. The simulator delegates the task
of displaying the output to gnuplot [@gnuplot_2017].

## Numerical algorithms to solve a system of differential equations

`Flint` compiles a model written in a supported XML language into internal
bytecode for simulation, and then evaluates it with particular initial values.
Our current implementation provides three algorithms for solving initial-value
problems for ODEs numerically: the forward Euler method, the Runge-Kutta
4th-order method, and the adaptive-step additive Runge-Kutta scheme implemented
in the SUNDIALS library [@hindmarsh2005sundials]. The Euler-Maruyama method is
used for solving SDEs [@higham_algorithmic_2001].

## Multithreading for parallel simulation

Solving an initial-value problem numerically is only the preliminary step to
analyze the dynamics of the model. Further investigation often asks for the
different values of initial values or parameters. For instance, hypotheses on
biological switches has been stated in terms of bifurcations, and demonstrated by
a series of simulations changing the values of parameters, in both deterministic
[@fussmann_crossing_2000] and stochastic [@samoilov_stochastic_2005] paradigms.
`Flint` employs multithreading to increase the number of simulations running in
parallel. The parallelization is automatically performed when the user assigns
multiple values to some parameter of a model for simulation, and honors the
number of available CPU cores, which can be adjusted in the preference.

## Grid search algorithm for parameter fitting

The larger the number of variables and parameters in a given model are, the more
resource-consuming its simulation becomes. It is also the case for estimating
plausible values of parameters consistent with the prior knowledge on the
behavior of the underlying system. Taking residual sum of squares (RSS) as a
measure of the goodness of fit, estimation of parameter values for ODEs turns
into a non-linear least-squares problem [@IMM2004-03215]. `Flint` deals with the
challenge the modeler faces when fitting the value of parameters via the
least-squares method, taking advantage of multithreading if available.
Given grid points in the parameter space as an input set, `Flint` performs the
following branch-and-bound algorithm to reduce the number of simultaneously
running jobs for the grid search:

![An algorithm for grid search to fit parameter values.\label{fig:algorithm}](algorithm.png)

Unlike existing heuristics for solving non-linear least-squares, the above
algorithm can find one of global minima, provided that the input grid contains
it. It is also easy to benefit from parallel computing to reduce processing
time. The necessary resource to be shared in parallel execution is only $m$ in
Fig. 1, namely a double floating-point number with its mutex, which means
the overhead is marginal. Users can define the range of each parameter as well
as the way to enumerate grid points, e.g. by a pseudo random number
generator. The feature will help researchers gain insight about a subset of
parameter values of biological/physiological interest at an early stage of
modeling.

# Acknowledgements

We acknowledge Dr Masao Okita for his invaluable comments on shared-memory
parallelism implemented in `Flint`.

# References
