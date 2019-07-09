AM_CPPFLAGS = -DTEST_MODELS_DIR='$(abs_top_srcdir)/test/models' -I$(top_srcdir)/test -I$(top_srcdir)/src/include -I$(top_srcdir)/src -I$(top_builddir)/src $(PROTOBUF_CFLAGS) $(BOOST_CPPFLAGS) $(CZMQ_CFLAGS) $(SQLITE3_CFLAGS)
AM_LDFLAGS = $(BOOST_UNIT_TEST_FRAMEWORK_LDFLAGS) $(BOOST_FILESYSTEM_LDFLAGS) $(BOOST_SYSTEM_LDFLAGS)
LDADD = ../../src/libflintxx.la ../../src/libflint.la $(PROTOBUF_LIBS) $(BOOST_UNIT_TEST_FRAMEWORK_LIBS) $(BOOST_FILESYSTEM_LIBS) $(BOOST_SYSTEM_LIBS) $(SQLITE3_LDFLAGS)
