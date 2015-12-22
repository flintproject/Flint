AM_CPPFLAGS = -DTEST_MODELS_DIR='$(abs_top_srcdir)/test/models' -I$(top_srcdir)/test -I$(top_srcdir)/src $(BOOST_CPPFLAGS)
AM_LDFLAGS = $(BOOST_UNIT_TEST_FRAMEWORK_LDFLAGS) $(BOOST_FILESYSTEM_LDFLAGS) $(BOOST_SYSTEM_LDFLAGS)
LDADD = ../../src/libflintxx.la ../../src/libflint.la ../../src/libsqlite3.la $(BOOST_UNIT_TEST_FRAMEWORK_LIBS) $(BOOST_FILESYSTEM_LIBS) $(BOOST_SYSTEM_LIBS)
