ACLOCAL_AMFLAGS = -I config

SUBDIRS = \
	lib \
	src \
	test

BUILT_SOURCES = \
	flint/src/jp/physiome/Cli.java \
	flint/src/jp/physiome/Ipc.java \
	flint/src/jp/physiome/Lo.java \
	flint/src/jp/physiome/Phml.java

flint/src/jp/physiome/Cli.java: pb/cli.proto
	$(PROTOC_EXECUTABLE) -I$(top_srcdir)/pb --java_out=$(top_srcdir)/flint/src $<

flint/src/jp/physiome/Ipc.java: pb/ipc.proto
	$(PROTOC_EXECUTABLE) -I$(top_srcdir)/pb --java_out=$(top_srcdir)/flint/src $<

flint/src/jp/physiome/Lo.java: pb/lo.proto
	$(PROTOC_EXECUTABLE) -I$(top_srcdir)/pb --java_out=$(top_srcdir)/flint/src $<

flint/src/jp/physiome/Phml.java: pb/phml.proto
	$(PROTOC_EXECUTABLE) -I$(top_srcdir)/pb --java_out=$(top_srcdir)/flint/src $<
