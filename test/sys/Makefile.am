include $(top_srcdir)/common.mk

TESTS = \
	test_temporary_path

noinst_PROGRAMS = $(TESTS)
AM_CPPFLAGS = -DBOOST_TEST_DYN_LINK -DBOOST_TEST_MAIN -I$(top_srcdir)/lib $(BOOST_CPPFLAGS) $(PROTOBUF_CFLAGS)
AM_LDFLAGS = $(BOOST_UNIT_TEST_FRAMEWORK_LDFLAGS)
LDADD = ../../lib/libissim.la $(BOOST_UNIT_TEST_FRAMEWORK_LIBS) $(PROTOBUF_LIBS)

test_temporary_path_SOURCES = test_temporary_path.cc
