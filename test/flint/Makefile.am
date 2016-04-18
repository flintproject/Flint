include $(top_srcdir)/common.mk
include $(top_srcdir)/test/test.mk

AM_CPPFLAGS += $(XML_CPPFLAGS)
LDADD += $(XML_LIBS)

TESTS = \
	test-cng \
	test-tokenizer \
	test-parser \
	test-trim

noinst_PROGRAMS = $(TESTS)

test_cng_SOURCES = test-cng.cc
test_tokenizer_SOURCES = test-tokenizer.cc
test_parser_SOURCES = test-parser.cc
test_trim_SOURCES = test-trim.cc
