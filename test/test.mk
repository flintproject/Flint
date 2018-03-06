AM_CPPFLAGS = -DTEST_MODELS_DIR='$(abs_top_srcdir)/test/models' -I$(top_srcdir)/test -I$(top_srcdir)/src/include -I$(top_srcdir)/src -I$(top_builddir)/src $(PROTOBUF_CFLAGS) $(BOOST_CPPFLAGS) $(CZMQ_CFLAGS)
AM_LDFLAGS = $(BOOST_UNIT_TEST_FRAMEWORK_LDFLAGS) $(BOOST_TIMER_LDFLAGS) $(BOOST_CHRONO_LDFLAGS) $(BOOST_FILESYSTEM_LDFLAGS) $(BOOST_SYSTEM_LDFLAGS)
LDADD = ../../src/libflintxx.la ../../src/libflint.la ../../src/libsqlite3.la $(PROTOBUF_LIBS) $(BOOST_UNIT_TEST_FRAMEWORK_LIBS) $(BOOST_TIMER_LIBS) $(BOOST_CHRONO_LIBS) $(BOOST_FILESYSTEM_LIBS) $(BOOST_SYSTEM_LIBS)
