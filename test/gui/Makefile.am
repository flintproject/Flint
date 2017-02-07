include $(top_srcdir)/common.mk
include $(top_srcdir)/test/test.mk

LDADD += ../../src/libflint2.a

TESTS = \
	test-formula-lexer \
	test-formula-parser

noinst_PROGRAMS = $(TESTS)

test_formula_lexer_SOURCES = test-formula-lexer.cc
test_formula_parser_SOURCES = test-formula-parser.cc
