include $(top_srcdir)/common.mk

TESTS = \
	test_math_dumper

noinst_PROGRAMS = $(TESTS)

test_math_dumper_CPPFLAGS = -DBOOST_TEST_DYN_LINK -DBOOST_TEST_MAIN -DTEST_XML_MODELS='$(top_srcdir)/test/models' -I$(top_srcdir)/src $(BOOST_CPPFLAGS) $(XML_CPPFLAGS)
test_math_dumper_LDFLAGS = $(BOOST_UNIT_TEST_FRAMEWORK_LDFLAGS)
test_math_dumper_LDADD = $(XML_LIBS) $(BOOST_UNIT_TEST_FRAMEWORK_LIBS)
test_math_dumper_SOURCES = test_math_dumper.cc
