include $(top_srcdir)/common.mk

lib_LTLIBRARIES = libissim.la

libissim_la_LDFLAGS = -no-undefined

libissim_la_LIBADD = \
	$(BOOST_FILESYSTEM_LDFLAGS) $(BOOST_FILESYSTEM_LIBS) \
	$(BOOST_SYSTEM_LDFLAGS) $(BOOST_SYSTEM_LIBS) \
	$(PROTOBUF_LIBS)

libissim_la_CPPFLAGS = \
	$(BOOST_CPPFLAGS) \
	$(PROTOBUF_CFLAGS)

libissim_la_SOURCES = \
    pb/ipc.pb.cc \
    sys/temporary_path.cc

pb/ipc.pb.cc pb/ipc.pb.h: $(top_srcdir)/pb/ipc.proto
	$(PROTOC_EXECUTABLE) -I$(top_srcdir)/pb --cpp_out=$(top_srcdir)/lib/pb $<
	$(PROTOC_EXECUTABLE) -I$(top_srcdir)/pb --java_out=$(top_srcdir)/flint/src $<
