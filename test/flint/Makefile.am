include $(top_srcdir)/common.mk
include $(top_srcdir)/test/test.mk

AM_CPPFLAGS += $(XML_CPPFLAGS)
LDADD += $(XML_LIBS)

TESTS = \
	test-cng \
	test-trim

noinst_PROGRAMS = $(TESTS)

test_cng_SOURCES = test-cng.cc
test_trim_SOURCES = test-trim.cc
