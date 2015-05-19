/* -*- Mode: C++; tab-width: 4; indent-tabs-mode: t; c-basic-offset: 4 -*- vim:set ts=4 sw=4 sts=4 noet: */
#include <cstdio>
#include <cstdlib>
#include <cstring>
#include <iostream>

#include <boost/test/unit_test.hpp>

#define TEST_MODELS_0(dirname, basename) (#dirname "/" basename)
#define TEST_MODELS_1(dirname, basename) TEST_MODELS_0(dirname, basename)
#define TEST_MODELS(basename) TEST_MODELS_1(TEST_MODELS_DIR, basename)
