include $(top_srcdir)/common.mk
include $(top_srcdir)/test/test.mk

AM_CPPFLAGS += $(XML_CPPFLAGS)
LDADD += $(XML_LIBS)

TESTS = \
	test-trim

noinst_PROGRAMS = $(TESTS)

test_trim_SOURCES = test-trim.cc
