include $(top_srcdir)/common.mk
include $(top_srcdir)/test/test.mk

AM_CPPFLAGS += $(XML_CPPFLAGS)
LDADD += $(XML_LIBS)

TESTS = \
	test-cng \
	test-sexp \
	test-sexp-parser \
	test-sexp-tokenizer \
	test-temporary-file \
	test-translate \
	test-trim

noinst_PROGRAMS = $(TESTS)

test_cng_SOURCES = test-cng.cc
test_sexp_SOURCES = test-sexp.cc
test_sexp_parser_SOURCES = test-sexp-parser.cc
test_sexp_tokenizer_SOURCES = test-sexp-tokenizer.cc
test_temporary_file_SOURCES = test-temporary-file.cc
test_translate_SOURCES = test-translate.cc
test_trim_SOURCES = test-trim.cc
