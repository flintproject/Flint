include $(top_srcdir)/common.mk

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
	flint-pause \
	flint-resume \
	flint-run \
	flint-tr \
	flint2 \
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
	fppp-isd \
	fppp-rtm \
	fppp-sim \
	fppp-sup \
	test-exec \
	test-run

# Dependency comes first
lib_LTLIBRARIES = libsqlite3.la libflint.la libflintxx.la

lib_LIBRARIES = libflint2.a

libflint_la_CPPFLAGS = $(AM_CFLAGS) -std=c99 -pedantic-errors
libflint_la_LIBADD = libsqlite3.la
libflint_la_LDFLAGS = -no-undefined
libflint_la_SOURCES = \
    database.c \
    db/query.c \
    process.c

libflintxx_la_CPPFLAGS = -I$(top_srcdir)/include $(SOSLIB_CFLAGS) $(LIBSBML_CFLAGS) $(WXWIDGETS_CXXFLAGS) $(PROTOBUF_CFLAGS) $(BOOST_CPPFLAGS) $(SUNDIALS_CFLAGS) $(CLIBSEDML_CFLAGS) $(CZMQ_CFLAGS) $(ZEROMQ_CFLAGS) $(XML_CPPFLAGS)
libflintxx_la_LIBADD = libflint.la libsqlite3.la $(SOSLIB_LIBS) $(LIBSBML_LIBS) $(WXWIDGETS_LIBS) $(PROTOBUF_LIBS) $(BOOST_FILESYSTEM_LIBS) $(BOOST_SYSTEM_LIBS) $(SUNDIALS_LIBS) $(CLIBSEDML_LIBS) $(CZMQ_LIBS) $(ZEROMQ_LIBS) $(XML_LIBS)
libflintxx_la_LDFLAGS = -no-undefined $(BOOST_FILESYSTEM_LDFLAGS) $(BOOST_SYSTEM_LDFLAGS)
libflintxx_la_SOURCES = \
    bc.pb.cc \
    cli.pb.cc \
    ipc.pb.cc \
    lo.pb.cc \
    phml.pb.cc \
    unit.pb.cc \
    background.cc \
    bc.cc \
    cas/dimension.cc \
    cas/eq.cc \
    cas/equation.cc \
    cas/helper.cc \
    cas/icg/aux-var.cc \
    cas/icg/ode-mass.cc \
    cas/icg/ode-rhs.cc \
    cas/printer.cc \
    cas/system.cc \
    cellml/parser.cc \
    cellml/read.cc \
    cellml/translator.cc \
    compiler.cc \
    compiler/bcc.cc \
    compiler/bcc/parser.cc \
    compiler/bcc/token.cc \
    compiler/bcc/tokenizer.cc \
    compiler/sort.cc \
    compiler/tac.cc \
    compiler/tac/context.cc \
    csv/export.cc \
    ct/dfa.cc \
    db/ast-inserter.cc \
    db/driver.cc \
    db/eq-inserter.cc \
    db/flow.cc \
    db/read-only-driver.cc \
    db/statement-driver.cc \
    db/tac-inserter.cc \
    db/variable-inserter.cc \
    dps.cc \
    exec/exec.cc \
    exec/job-runner.cc \
    exec/parameter.cc \
    exec/progress.cc \
    exec/task-runner.cc \
    file/detect.cc \
    file/txt.cc \
    filter/create.cc \
    filter/isdh.cc \
    filter/track.cc \
    filter/writer.cc \
    fppp.cc \
    fppp/url.cc \
    isd2csv.cc \
    isdstrip.cc \
    job/evolve.cc \
    job/generate.cc \
    job/job.cc \
    job/path.cc \
    job/store.cc \
    key.cc \
    layout/generate.cc \
    lo/layout.cc \
    load/load.cc \
    load/param.cc \
    load/var.cc \
    ls.cc \
    mathml/math-dumper.cc \
    mathml/mathml.cc \
    method/assign.cc \
    method/euler.cc \
    method/event.cc \
    method/helper.cc \
    method/printer.cc \
    method/rk4.cc \
    modelpath.cc \
    phml/branch.cc \
    phml/combine-all.cc \
    phml/combine.cc \
    phml/database-driver.cc \
    phml/delay-arg-validator.cc \
    phml/extra-implementation.cc \
    phml/graph-iv-rewriter.cc \
    phml/graph-math-rewriter.cc \
    phml/graph-reader.cc \
    phml/implementation.cc \
    phml/import.cc \
    phml/initial-value.cc \
    phml/lands.cc \
    phml/nc.cc \
    phml/reach.cc \
    phml/read.cc \
    phml/span.cc \
    phml/sprinkle.cc \
    phml/target-pq.cc \
    phml/transition-form.cc \
    phml/tsipc.cc \
    phml/unit-of-time.cc \
    phml/validator.cc \
    phsp/read.cc \
    phsp/sample-builder.cc \
    phz/read.cc \
    reduction.cc \
    run/run.cc \
    run/spec.cc \
    runtime/calculation-dependency.cc \
    runtime/calculation-unit.cc \
    runtime/channel.cc \
    runtime/eval.cc \
    runtime/evaluator.cc \
    runtime/flow.cc \
    runtime/history.cc \
    runtime/init.cc \
    runtime/matrix.cc \
    runtime/reduction-unit.cc \
    runtime/section-context.cc \
    runtime/timeseries.cc \
    sbml/load.cc \
    sbml/parser.cc \
    sbml/read.cc \
    sedml/read.cc \
    sexp.cc \
    sexp/parser.cc \
    sexp/token.cc \
    sexp/tokenizer.cc \
    solver/ark/ark.cc \
    solver/ark/auxv.cc \
    solver/ark/mass.cc \
    solver/ark/mmdm.cc \
    solver/ark/rhs.cc \
    solver/ark/solve.cc \
    solver/ark/user-supplied.cc \
    solver/solve.cc \
    stats.cc \
    sys/temporary_path.cc \
    task/config-reader.cc \
    task/config.cc \
    task/form.cc \
    task/spec.cc \
    task/timer.cc \
    tr/translate.cc \
    tr/translator.cc \
    ts/tsc.cc \
    ts/tsfiles.cc \
    utf8path.cc \
    utf8string.c \
    uuidgen.cc \
    variable.cc \
    variable-map.cc \
    workspace.cc

libflint2_a_SOURCES = \
    gui/formula.cc \
    gui/formula/lexer.cc \
    gui/formula/parser.cc \
    gui/formula/token.cc \
    gui/formula/tree.cc

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

flint_exec_CPPFLAGS = $(PROTOBUF_CFLAGS) $(BOOST_CPPFLAGS)
flint_exec_LDFLAGS = $(BOOST_FILESYSTEM_LDFLAGS) $(BOOST_SYSTEM_LDFLAGS)
flint_exec_LDADD = libflintxx.la libflint.la libsqlite3.la $(PROTOBUF_LIBS) $(BOOST_FILESYSTEM_LIBS) $(BOOST_SYSTEM_LIBS)
flint_exec_SOURCES = flint-exec-main.cc

flint_lodbg_CPPFLAGS = $(PROTOBUF_CFLAGS) $(BOOST_CPPFLAGS) $(CZMQ_CFLAGS)
flint_lodbg_LDADD = libflintxx.la libflint.la libsqlite3.la $(PROTOBUF_LIBS) $(CZMQ_LIBS)
flint_lodbg_SOURCES = flint-lodbg-main.cc

flint_open_CPPFLAGS = $(PROTOBUF_CFLAGS) $(BOOST_CPPFLAGS)
flint_open_LDADD = libflintxx.la libflint.la libsqlite3.la $(PROTOBUF_LIBS)
flint_open_SOURCES = flint-open-main.cc

flint_pause_SOURCES = flint-pause-main.c

flint_resume_SOURCES = flint-resume-main.c

flint_run_CPPFLAGS = $(PROTOBUF_CFLAGS) $(BOOST_CPPFLAGS)
flint_run_LDFLAGS = $(BOOST_FILESYSTEM_LDFLAGS) $(BOOST_SYSTEM_LDFLAGS)
flint_run_LDADD = libflintxx.la libflint.la libsqlite3.la $(PROTOBUF_LIBS) $(BOOST_FILESYSTEM_LIBS) $(BOOST_SYSTEM_LIBS)
flint_run_SOURCES = flint-run-main.cc

flint_tr_CPPFLAGS = $(PROTOBUF_CFLAGS) $(BOOST_CPPFLAGS)
flint_tr_LDFLAGS = $(BOOST_FILESYSTEM_LDFLAGS) $(BOOST_SYSTEM_LDFLAGS)
flint_tr_LDADD = libflintxx.la libflint.la libsqlite3.la $(PROTOBUF_LIBS) $(BOOST_FILESYSTEM_LIBS) $(BOOST_SYSTEM_LIBS)
flint_tr_SOURCES = flint-tr-main.cc

flint2_CPPFLAGS = $(WXWIDGETS_CXXFLAGS) $(PROTOBUF_CFLAGS) $(BOOST_CPPFLAGS)
flint2_LDFLAGS = $(BOOST_FILESYSTEM_LDFLAGS) $(BOOST_SYSTEM_LDFLAGS)
flint2_LDADD = libflint2.a libflintxx.la libflint.la libsqlite3.la $(WXWIDGETS_LIBS) $(PROTOBUF_LIBS) $(BOOST_FILESYSTEM_LIBS) $(BOOST_SYSTEM_LIBS)
flint2_SOURCES = \
    gui/app.cc \
    gui/configuration.cc \
    gui/document.cc \
    gui/document/unit-of-time.cc \
    gui/export-all-dialog.cc \
    gui/gnuplot.cc \
    gui/job.cc \
    gui/main-frame.cc \
    gui/param-tree.cc \
    gui/phsp-editor-dialog.cc \
    gui/phsp.cc \
    gui/pref-page-general.cc \
    gui/sedml.cc \
    gui/sim-frame.cc \
    gui/simulation.cc \
    gui/sub-window.cc \
    gui/task-frame.cc \
    gui/task-gauge.cc \
    gui/task.cc \
    gui/view-frame.cc

isd2csv_CPPFLAGS = $(BOOST_CPPFLAGS)
isd2csv_LDFLAGS = $(BOOST_PROGRAM_OPTIONS_LDFLAGS)
isd2csv_LDADD = libflintxx.la $(BOOST_PROGRAM_OPTIONS_LIBS)
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

fppp_isd_CPPFLAGS = $(BOOST_CPPFLAGS) $(CZMQ_CFLAGS)
fppp_isd_LDADD = libflintxx.la libflint.la libsqlite3.la $(CZMQ_LIBS)
fppp_isd_SOURCES = fppp/isd/main.cc

fppp_rtm_CPPFLAGS = $(BOOST_CPPFLAGS) $(CZMQ_CFLAGS) $(ZEROMQ_CFLAGS)
fppp_rtm_LDADD = libflintxx.la libflint.la libsqlite3.la $(CZMQ_LIBS)
fppp_rtm_SOURCES = fppp/rtm/main.cc

fppp_sim_CPPFLAGS = $(PROTOBUF_CFLAGS) $(BOOST_CPPFLAGS) $(CZMQ_CFLAGS) $(ZEROMQ_CFLAGS)
fppp_sim_LDADD = libflintxx.la libflint.la libsqlite3.la $(PROTOBUF_LIBS) $(CZMQ_LIBS)
fppp_sim_SOURCES = fppp/sim/main.cc

fppp_sup_CPPFLAGS = $(BOOST_CPPFLAGS) $(CZMQ_CFLAGS) $(ZEROMQ_CFLAGS)
fppp_sup_LDADD = libflintxx.la libflint.la libsqlite3.la $(CZMQ_LIBS)
fppp_sup_SOURCES = fppp/sup/main.cc

test_exec_CPPFLAGS = $(PROTOBUF_CFLAGS) $(BOOST_CPPFLAGS)
test_exec_LDADD = libflintxx.la libflint.la libsqlite3.la $(PROTOBUF_LIBS)
test_exec_SOURCES = test-exec-main.cc

test_run_CPPFLAGS = $(PROTOBUF_CFLAGS) $(BOOST_CPPFLAGS)
test_run_LDADD = libflintxx.la libflint.la libsqlite3.la $(PROTOBUF_LIBS)
test_run_SOURCES = test-run-main.cc
