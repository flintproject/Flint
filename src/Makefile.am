include $(top_srcdir)/common.mk

.h.hh:
	install -m 0644 $< $@

bc.pb.cc bc.pb.h: $(top_srcdir)/pb/bc.proto
	$(PROTOC_EXECUTABLE) -I$(top_srcdir)/pb --cpp_out=./ $<

cli.pb.cc cli.pb.h: $(top_srcdir)/pb/cli.proto
	$(PROTOC_EXECUTABLE) -I$(top_srcdir)/pb --cpp_out=./ $<

ipc.pb.cc ipc.pb.h: $(top_srcdir)/pb/ipc.proto
	$(PROTOC_EXECUTABLE) -I$(top_srcdir)/pb --cpp_out=./ $<

lo.pb.cc lo.pb.h: $(top_srcdir)/pb/lo.proto
	$(PROTOC_EXECUTABLE) -I$(top_srcdir)/pb --cpp_out=./ $<

phml.pb.cc phml.pb.h: $(top_srcdir)/pb/phml.proto
	$(PROTOC_EXECUTABLE) -I$(top_srcdir)/pb --cpp_out=./ $<

unit.pb.cc unit.pb.h: $(top_srcdir)/pb/unit.proto
	$(PROTOC_EXECUTABLE) -I$(top_srcdir)/pb --cpp_out=./ $<

BUILT_SOURCES = \
    bc.pb.h \
    cli.pb.h \
    ipc.pb.h \
    lo.pb.h \
    phml.pb.h \
    unit.pb.h

bin_PROGRAMS = \
	csv2isd \
	flint-cli \
	flint-exec \
	flint-lodbg \
	flint-open \
	flint-run \
	isd2csv \
	isdbswap \
	isdcut \
	isddiff \
	isdhead \
	isdls \
	isdmerge \
	isdplot \
	isdrand \
	isdsort \
	isdstrip \
	isdwc

noinst_PROGRAMS = \
	test-run

# Dependency comes first
lib_LTLIBRARIES = libsqlite3.la libflint.la libflintxx.la

libflint_la_CPPFLAGS = $(AM_CFLAGS) -std=c99 -pedantic-errors
libflint_la_LIBADD = libsqlite3.la
libflint_la_LDFLAGS = -no-undefined
libflint_la_SOURCES = \
    database.c \
    db/query.c \
    system.c

libflintxx_la_CPPFLAGS = $(BOOST_CPPFLAGS) $(PROTOBUF_CFLAGS) $(SOSLIB_CFLAGS) $(SUNDIALS_CFLAGS) $(LIBSBML_CFLAGS) $(CLIBSEDML_CFLAGS) $(XML_CPPFLAGS) $(LIBZIP_CFLAGS)
libflintxx_la_LIBADD = libflint.la libsqlite3.la $(BOOST_FILESYSTEM_LIBS) $(BOOST_SYSTEM_LIBS) $(PROTOBUF_LIBS) $(SOSLIB_LIBS) $(SUNDIALS_LIBS) $(LIBSBML_LIBS) $(CLIBSEDML_LIBS) $(XML_LIBS) $(LIBZIP_LIBS)
libflintxx_la_LDFLAGS = -no-undefined $(BOOST_FILESYSTEM_LDFLAGS) $(BOOST_SYSTEM_LDFLAGS)
libflintxx_la_SOURCES = \
    bc.pb.cc \
    cli.pb.cc \
    ipc.pb.cc \
    lo.pb.cc \
    phml.pb.cc \
    unit.pb.cc \
    branch.cc \
    cellml/parser.cc \
    cellml/read.cc \
    cellml/translator.cc \
    compiler/bcc.cc \
    compiler/compile.cc \
    compiler/sort.cc \
    compiler/tac.cc \
    csv/export.cc \
    db/driver.cc \
    db/eq-inserter.cc \
    db/flow.cc \
    db/name-inserter.cc \
    db/read-only-driver.cc \
    db/statement-driver.cc \
    db/tac-inserter.cc \
    exec/enum.cc \
    exec/exec.cc \
    exec/job-runner.cc \
    exec/task-runner.cc \
    file/detect.cc \
    file/txt.cc \
    filter/create.cc \
    filter/cut.cc \
    filter/isdh.cc \
    filter/track.cc \
    filter/writer.cc \
    job/evolve.cc \
    job/generate.cc \
    job/job.cc \
    job/store.cc \
    layout/generate.cc \
    lo/layout.cc \
    load/load.cc \
    load/param.cc \
    load/var.cc \
    method/assign.cc \
    method/euler.cc \
    method/event.cc \
    method/helper.cc \
    method/rk4.cc \
    modelpath.cc \
    phml/combine-all.cc \
    phml/combine.cc \
    phml/delay-arg-validator.cc \
    phml/graph-iv-rewriter.cc \
    phml/graph-math-rewriter.cc \
    phml/import.cc \
    phml/lands.cc \
    phml/nc.cc \
    phml/read.cc \
    phml/transition-form.cc \
    phml/unit-of-time.cc \
    phsp/read.cc \
    phz/read.cc \
    reach.cc \
    reduction.cc \
    run/run.cc \
    run/spec.cc \
    runtime/calculation-dependency.cc \
    runtime/init.cc \
    runtime/flow.cc \
    runtime/reduction-unit.cc \
    sbml/load.cc \
    sbml/parser.cc \
    sbml/read.cc \
    sedml/read.cc \
    span.cc \
    sprinkle.cc \
    sys/temporary_path.cc \
    task/config.cc \
    task/config-reader.cc \
    task/form.cc \
    task/spec.cc \
    task/timer.cc \
    ts/tsc.cc \
    ts/tsfiles.cc \
    utf8path.cc \
    uuidgen.cc

libsqlite3_la_CPPFLAGS = -DSQLITE_THREADSAFE=1 -DSQLITE_OMIT_LOAD_EXTENSION
libsqlite3_la_LDFLAGS = -no-undefined
libsqlite3_la_SOURCES = sqlite3.c

csv2isd_CPPFLAGS = $(BOOST_CPPFLAGS)
csv2isd_LDFLAGS = $(BOOST_FILESYSTEM_LDFLAGS) $(BOOST_SYSTEM_LDFLAGS)
csv2isd_LDADD = libflintxx.la libflint.la libsqlite3.la $(BOOST_FILESYSTEM_LIBS) $(BOOST_SYSTEM_LIBS)
csv2isd_SOURCES = csv2isd-main.cc

flint_cli_CPPFLAGS = $(PROTOBUF_CFLAGS) $(BOOST_CPPFLAGS)
flint_cli_LDFLAGS = $(BOOST_PROGRAM_OPTIONS_LDFLAGS)
flint_cli_LDADD = libflintxx.la libflint.la libsqlite3.la $(PROTOBUF_LIBS) $(BOOST_PROGRAM_OPTIONS_LIBS)
flint_cli_SOURCES = flint-cli-main.cc

flint_exec_CPPFLAGS = $(BOOST_CPPFLAGS)
flint_exec_LDFLAGS = $(BOOST_FILESYSTEM_LDFLAGS) $(BOOST_SYSTEM_LDFLAGS)
flint_exec_LDADD = libflintxx.la libflint.la libsqlite3.la $(BOOST_FILESYSTEM_LIBS) $(BOOST_SYSTEM_LIBS)
flint_exec_SOURCES = flint-exec-main.cc

flint_lodbg_CPPFLAGS = $(PROTOBUF_CFLAGS) $(BOOST_CPPFLAGS)
flint_lodbg_LDADD = libflintxx.la libflint.la libsqlite3.la $(PROTOBUF_LIBS)
flint_lodbg_SOURCES = flint-lodbg-main.cc

flint_open_CPPFLAGS = $(BOOST_CPPFLAGS)
flint_open_LDADD = libflintxx.la libflint.la libsqlite3.la
flint_open_SOURCES = flint-open-main.cc

flint_run_CPPFLAGS = $(PROTOBUF_CFLAGS) $(BOOST_CPPFLAGS)
flint_run_LDFLAGS = $(BOOST_FILESYSTEM_LDFLAGS) $(BOOST_SYSTEM_LDFLAGS)
flint_run_LDADD = libflintxx.la libflint.la libsqlite3.la $(PROTOBUF_LIBS) $(BOOST_FILESYSTEM_LIBS) $(BOOST_SYSTEM_LIBS)
flint_run_SOURCES = flint-run-main.cc

isd2csv_CPPFLAGS = $(BOOST_CPPFLAGS)
isd2csv_LDFLAGS = $(BOOST_PROGRAM_OPTIONS_LDFLAGS)
isd2csv_LDADD = $(BOOST_PROGRAM_OPTIONS_LIBS)
if ENABLE_TCP
isd2csv_LDFLAGS += $(BOOST_SYSTEM_LDFLAGS)
isd2csv_LDADD += $(BOOST_SYSTEM_LIBS)
endif
isd2csv_SOURCES = isd2csv-main.cc

isdbswap_SOURCES = isdbswap-main.c

isdcut_CPPFLAGS = $(BOOST_CPPFLAGS)
isdcut_LDFLAGS = $(BOOST_PROGRAM_OPTIONS_LDFLAGS)
isdcut_LDADD = $(BOOST_PROGRAM_OPTIONS_LIBS)
isdcut_SOURCES = isdcut-main.cc

isddiff_CPPFLAGS = $(BOOST_CPPFLAGS)
isddiff_LDFLAGS = $(BOOST_PROGRAM_OPTIONS_LDFLAGS)
isddiff_LDADD = $(BOOST_PROGRAM_OPTIONS_LIBS)
isddiff_SOURCES = isddiff-main.cc

isdhead_CPPFLAGS = $(BOOST_CPPFLAGS)
isdhead_LDFLAGS = $(BOOST_PROGRAM_OPTIONS_LDFLAGS)
isdhead_LDADD = $(BOOST_PROGRAM_OPTIONS_LIBS)
isdhead_SOURCES = isdhead-main.cc

isdls_CPPFLAGS = $(BOOST_CPPFLAGS)
isdls_LDFLAGS = $(BOOST_PROGRAM_OPTIONS_LDFLAGS)
isdls_LDADD = $(BOOST_PROGRAM_OPTIONS_LIBS)
isdls_SOURCES = isdls-main.cc

isdmerge_CPPFLAGS = $(BOOST_CPPFLAGS)
isdmerge_LDFLAGS = $(BOOST_PROGRAM_OPTIONS_LDFLAGS)
isdmerge_LDADD = $(BOOST_PROGRAM_OPTIONS_LIBS)
isdmerge_SOURCES = isdmerge-main.cpp

isdplot_CPPFLAGS = $(BOOST_CPPFLAGS)
isdplot_LDFLAGS = $(BOOST_FILESYSTEM_LDFLAGS) $(BOOST_SYSTEM_LDFLAGS) $(BOOST_PROGRAM_OPTIONS_LDFLAGS)
isdplot_LDADD = libflintxx.la libflint.la libsqlite3.la $(BOOST_FILESYSTEM_LIBS) $(BOOST_SYSTEM_LIBS) $(BOOST_PROGRAM_OPTIONS_LIBS)
isdplot_SOURCES = isdplot-main.cc

isdrand_CFLAGS = $(AM_CFLAGS) -std=c99 -pedantic-errors
isdrand_SOURCES = isdrand-main.c

isdsort_CPPFLAGS = $(BOOST_CPPFLAGS)
isdsort_SOURCES = isdsort-main.cc

isdstrip_CPPFLAGS = $(BOOST_CPPFLAGS)
isdstrip_LDFLAGS = $(BOOST_FILESYSTEM_LDFLAGS) $(BOOST_SYSTEM_LDFLAGS) $(BOOST_PROGRAM_OPTIONS_LDFLAGS)
isdstrip_LDADD = libflintxx.la libflint.la libsqlite3.la $(BOOST_FILESYSTEM_LIBS) $(BOOST_SYSTEM_LIBS) $(BOOST_PROGRAM_OPTIONS_LIBS)
isdstrip_SOURCES = isdstrip-main.cc

isdwc_CPPFLAGS = $(BOOST_CPPFLAGS)
isdwc_LDFLAGS = $(BOOST_PROGRAM_OPTIONS_LDFLAGS)
isdwc_LDADD = $(BOOST_PROGRAM_OPTIONS_LIBS)
isdwc_SOURCES = isdwc-main.cc

test_run_CPPFLAGS = $(PROTOBUF_CFLAGS) $(BOOST_CPPFLAGS)
test_run_LDADD = libflintxx.la libflint.la libsqlite3.la $(PROTOBUF_LIBS)
test_run_SOURCES = test-run-main.cc
