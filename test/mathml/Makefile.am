include $(top_srcdir)/common.mk
include $(top_srcdir)/test/test.mk

TESTS = \
	test_math_dumper

noinst_PROGRAMS = $(TESTS)

test_math_dumper_CPPFLAGS = $(AM_CPPFLAGS) $(XML_CPPFLAGS)
test_math_dumper_LDADD = $(LDADD) $(XML_LIBS)
test_math_dumper_SOURCES = test_math_dumper.cc
