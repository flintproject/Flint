include $(top_srcdir)/common.mk
include $(top_srcdir)/test/test.mk

TESTS = \
	test-eq-determinant \
	test-eq-outerproduct \
	test-eq-scalarproduct \
	test-eq-selector \
	test-eq-transpose \
	test-eq-vectorproduct \
	test-equations

noinst_PROGRAMS = $(TESTS)

test_eq_determinant_SOURCES = test-eq-determinant.cc
test_eq_outerproduct_SOURCES = test-eq-outerproduct.cc
test_eq_scalarproduct_SOURCES = test-eq-scalarproduct.cc
test_eq_selector_SOURCES = test-eq-selector.cc
test_eq_transpose_SOURCES = test-eq-transpose.cc
test_eq_vectorproduct_SOURCES = test-eq-vectorproduct.cc
test_equations_SOURCES = test-equations.cc
