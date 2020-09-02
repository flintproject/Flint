Contributing to Flint
=====================

This document describes an overview of Flint's development and how to contribute
it. First of all, using Flint for your own research is one of the best
contribution to the Flint project. Joining its development is another.

Goals
-----

We value the following features of software in terms of making Flint a practical
tool.
* Accuracy.
  Inaccurate simulation serves no purpose.
* Simplicity.
  A comprehensible interface, rather than comprehensive, is what users really need.
* Open source and open standard.
  Closed or proprietary part of simulation causes irreproducibility.
* Portability.
  A simulator should run on computers owned by students, who later become researchers.
* Sustainability.
  No one want to use a simulator that will be unmaintained once its grant finishes.

Architecture
------------

We concern with the following traits of the simulator's architecture in order to
achieve the above goals.
* No plugins.
* Input can be in various XML format; the output format is CSV.
* Keep run-time/compile-time dependencies as small as possible.
* Choose dependency libraries that have stable history of development.

Practice
--------

The following general rules apply for our collaborative development.
* Open a pull request on <https://github.com/flintproject/Flint/pulls>.
* Run tests before submitting a pull request.
* Augment tests so that any functional changes made are also tested.
* Follow the master branch of the git repository, on which next release is based.
* The release cycle is intended to be six months.
* Optionally, subscribe the official mailing list at
  <https://groups.google.com/g/flint-discuss>.
  Read <https://support.google.com/groups/answer/1067205> about how to join it.
  You can browse its forum without any Google account:
  <https://groups.google.com/forum/#!forum/flint-discuss>.
