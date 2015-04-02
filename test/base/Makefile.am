include $(top_srcdir)/common.mk

TESTS = \
	test_rational

noinst_PROGRAMS = $(TESTS)
AM_CPPFLAGS = -DBOOST_TEST_DYN_LINK -DBOOST_TEST_MAIN -I$(top_srcdir)/src $(BOOST_CPPFLAGS)
AM_LDFLAGS = $(BOOST_UNIT_TEST_FRAMEWORK_LDFLAGS)
LDADD = $(BOOST_UNIT_TEST_FRAMEWORK_LIBS)

test_rational_SOURCES = test_rational.cc
