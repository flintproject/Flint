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
	flint-cellml \
	flint-cli \
	flint-compile \
	flint-concat \
	flint-cut \
	flint-enum \
	flint-evolve \
	flint-exec \
	flint-filter \
	flint-form \
	flint-generate \
	flint-init \
	flint-isdh \
	flint-lands \
	flint-layout \
	flint-loadconfig \
	flint-lodbg \
	flint-nc \
	flint-open \
	flint-param \
	flint-phml \
	flint-phsp \
	flint-phz \
	flint-run \
	flint-sbml \
	flint-sedml \
	flint-spec \
	flint-store \
	flint-taskconfig \
	flint-taskpref \
	flint-taskspec \
	flint-timer \
	flint-track \
	flint-unitoftime \
	flint-var \
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
	test-input1 \
	test-input2 \
	test-run \
	test-singleton

lib_LTLIBRARIES = libflint.la libflintxx.la libsqlite3.la

libflint_la_CPPFLAGS = $(AM_CFLAGS) -std=c99 -pedantic-errors
libflint_la_LIBADD = libsqlite3.la
libflint_la_LDFLAGS = -no-undefined
libflint_la_SOURCES = \
    database.c \
    db/query.c \
    system.c

libflintxx_la_CPPFLAGS = $(BOOST_CPPFLAGS) $(PROTOBUF_CFLAGS) $(SOSLIB_CFLAGS) $(SUNDIALS_CFLAGS) $(LIBSBML_CFLAGS) $(XML_CPPFLAGS)
libflintxx_la_LIBADD = libflint.la libsqlite3.la $(BOOST_FILESYSTEM_LIBS) $(BOOST_SYSTEM_LIBS) $(PROTOBUF_LIBS) $(SOSLIB_LIBS) $(SUNDIALS_LIBS) $(LIBSBML_LIBS) $(XML_LIBS)
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
    cellml/translator.cc \
    db/flow.cc \
    db/read-only-driver.cc \
    compiler/bcc.cc \
    compiler/sort.cc \
    compiler/tac.cc \
    csv/export.cc \
    file/detect.cc \
    file/txt.cc \
    job/generate.cc \
    method/assign.cc \
    method/euler.cc \
    method/event.cc \
    method/rk4.cc \
    modelpath.cc \
    phml/combine-all.cc \
    phml/combine.cc \
    phml/graph-iv-rewriter.cc \
    phml/graph-math-rewriter.cc \
    phml/import.cc \
    phml/read.cc \
    phml/transition-form.cc \
    reach.cc \
    runtime/flow.cc \
    sbml/load.cc \
    sbml/parser.cc \
    span.cc \
    sprinkle.cc \
    sys/temporary_path.cc \
    ts/tsc.cc \
    ts/tsfiles.cc \
    utf8path.cc \
    uuidgen.cc \
    workspace/task.cc

libsqlite3_la_CPPFLAGS = -DSQLITE_THREADSAFE=0 -DSQLITE_OMIT_LOAD_EXTENSION
libsqlite3_la_LDFLAGS = -no-undefined
libsqlite3_la_SOURCES = sqlite3.c

csv2isd_CPPFLAGS = $(BOOST_CPPFLAGS)
csv2isd_LDFLAGS = $(BOOST_FILESYSTEM_LDFLAGS) $(BOOST_SYSTEM_LDFLAGS)
csv2isd_LDADD = libflintxx.la libflint.la libsqlite3.la $(BOOST_FILESYSTEM_LIBS) $(BOOST_SYSTEM_LIBS)
csv2isd_SOURCES = csv2isd-main.cc

flint_cellml_CPPFLAGS = $(BOOST_CPPFLAGS)
flint_cellml_LDFLAGS = $(BOOST_FILESYSTEM_LDFLAGS) $(BOOST_SYSTEM_LDFLAGS)
flint_cellml_LDADD = libflintxx.la libflint.la libsqlite3.la $(BOOST_FILESYSTEM_LIBS) $(BOOST_SYSTEM_LIBS)
flint_cellml_SOURCES = flint-cellml-main.cc

flint_cli_CPPFLAGS = $(PROTOBUF_CFLAGS) $(BOOST_CPPFLAGS)
flint_cli_LDFLAGS = $(BOOST_PROGRAM_OPTIONS_LDFLAGS)
flint_cli_LDADD = libflintxx.la libflint.la libsqlite3.la $(PROTOBUF_LIBS) $(BOOST_PROGRAM_OPTIONS_LIBS)
flint_cli_SOURCES = flint-cli-main.cc

flint_compile_CPPFLAGS = $(PROTOBUF_CFLAGS) $(BOOST_CPPFLAGS)
flint_compile_LDADD = libflintxx.la libflint.la libsqlite3.la $(PROTOBUF_LIBS)
flint_compile_SOURCES = flint-compile-main.cc

flint_concat_CFLAGS = $(AM_CFLAGS) -std=c99 -pedantic-errors
flint_concat_SOURCES = flint-concat-main.c

flint_cut_CPPFLAGS = $(PROTOBUF_CFLAGS) $(BOOST_CPPFLAGS)
flint_cut_LDFLAGS = $(BOOST_PROGRAM_OPTIONS_LDFLAGS) $(BOOST_SYSTEM_LDFLAGS)
flint_cut_LDADD = libflintxx.la libflint.la libsqlite3.la $(PROTOBUF_LIBS) $(BOOST_PROGRAM_OPTIONS_LIBS) $(BOOST_SYSTEM_LIBS)
flint_cut_SOURCES = flint-cut-main.cc

flint_enum_CFLAGS = $(AM_CFLAGS) -std=c99 -pedantic-errors
flint_enum_LDADD = libflint.la libsqlite3.la
flint_enum_SOURCES = flint-enum-main.c

flint_evolve_CPPFLAGS = $(PROTOBUF_CFLAGS) $(BOOST_CPPFLAGS)
flint_evolve_LDFLAGS = $(BOOST_PROGRAM_OPTIONS_LDFLAGS) $(BOOST_FILESYSTEM_LDFLAGS) $(BOOST_SYSTEM_LDFLAGS)
flint_evolve_LDADD = libflintxx.la libflint.la libsqlite3.la $(PROTOBUF_LIBS) $(BOOST_PROGRAM_OPTIONS_LIBS) $(BOOST_FILESYSTEM_LIBS) $(BOOST_SYSTEM_LIBS)
flint_evolve_SOURCES = flint-evolve-main.cc

flint_exec_CPPFLAGS = $(BOOST_CPPFLAGS)
flint_exec_LDFLAGS = $(BOOST_FILESYSTEM_LDFLAGS) $(BOOST_SYSTEM_LDFLAGS)
flint_exec_LDADD = libflint.la libsqlite3.la $(BOOST_FILESYSTEM_LIBS) $(BOOST_SYSTEM_LIBS)
flint_exec_SOURCES = flint-exec-main.cc

flint_filter_CPPFLAGS = $(PROTOBUF_CFLAGS) $(BOOST_CPPFLAGS)
flint_filter_LDFLAGS = $(BOOST_PROGRAM_OPTIONS_LDFLAGS) $(BOOST_SYSTEM_LDFLAGS)
flint_filter_LDADD = libflintxx.la libflint.la libsqlite3.la $(PROTOBUF_LIBS) $(BOOST_PROGRAM_OPTIONS_LIBS) $(BOOST_SYSTEM_LIBS)
flint_filter_SOURCES = flint-filter-main.cc

flint_form_CFLAGS = $(AM_CFLAGS) -std=c99 -pedantic-errors
flint_form_LDADD = libflint.la libsqlite3.la
flint_form_SOURCES = flint-form-main.c

flint_generate_CPPFLAGS = $(BOOST_CPPFLAGS)
flint_generate_LDADD = libflintxx.la libflint.la libsqlite3.la
flint_generate_SOURCES = flint-generate-main.cc

flint_init_CPPFLAGS = $(PROTOBUF_CFLAGS) $(BOOST_CPPFLAGS)
flint_init_LDFLAGS = $(BOOST_PROGRAM_OPTIONS_LDFLAGS) $(BOOST_FILESYSTEM_LDFLAGS) $(BOOST_SYSTEM_LDFLAGS)
flint_init_LDADD = libflintxx.la libflint.la libsqlite3.la $(PROTOBUF_LIBS) $(BOOST_PROGRAM_OPTIONS_LIBS) $(BOOST_FILESYSTEM_LIBS) $(BOOST_SYSTEM_LIBS)
flint_init_SOURCES = flint-init-main.cc

flint_isdh_CPPFLAGS = $(PROTOBUF_CFLAGS) $(BOOST_CPPFLAGS)
flint_isdh_LDFLAGS = $(BOOST_PROGRAM_OPTIONS_LDFLAGS)
flint_isdh_LDADD = libflintxx.la libflint.la libsqlite3.la $(PROTOBUF_LIBS) $(BOOST_PROGRAM_OPTIONS_LIBS)
flint_isdh_SOURCES = flint-isdh-main.cc

flint_lands_CPPFLAGS = $(PROTOBUF_CFLAGS) $(BOOST_CPPFLAGS)
flint_lands_LDFLAGS = $(BOOST_PROGRAM_OPTIONS_LDFLAGS)
flint_lands_LDADD = libflintxx.la libflint.la libsqlite3.la $(PROTOBUF_LIBS) $(BOOST_PROGRAM_OPTIONS_LIBS)
flint_lands_SOURCES = flint-lands-main.cc

flint_layout_CPPFLAGS = $(PROTOBUF_CFLAGS) $(BOOST_CPPFLAGS)
flint_layout_LDFLAGS = $(BOOST_PROGRAM_OPTIONS_LDFLAGS)
flint_layout_LDADD = libflintxx.la libflint.la libsqlite3.la $(PROTOBUF_LIBS) $(BOOST_PROGRAM_OPTIONS_LIBS)
flint_layout_SOURCES = flint-layout-main.cc

flint_loadconfig_CPPFLAGS = $(AM_CFLAGS) -std=c99 -pedantic-errors
flint_loadconfig_SOURCES = flint-loadconfig-main.c

flint_lodbg_CPPFLAGS = $(PROTOBUF_CFLAGS) $(BOOST_CPPFLAGS)
flint_lodbg_LDADD = libflintxx.la libflint.la libsqlite3.la $(PROTOBUF_LIBS)
flint_lodbg_SOURCES = flint-lodbg-main.cc

flint_nc_CPPFLAGS = $(PROTOBUF_CFLAGS) $(BOOST_CPPFLAGS)
flint_nc_LDADD = libflintxx.la libflint.la libsqlite3.la $(PROTOBUF_LIBS)
flint_nc_SOURCES = flint-nc-main.cc

flint_open_CPPFLAGS = $(BOOST_CPPFLAGS)
flint_open_LDADD = libflintxx.la libflint.la libsqlite3.la
flint_open_SOURCES = flint-open-main.cc

flint_param_CPPFLAGS = $(PROTOBUF_CFLAGS) $(BOOST_CPPFLAGS)
flint_param_LDADD = libflintxx.la libflint.la libsqlite3.la $(PROTOBUF_LIBS)
flint_param_SOURCES = flint-param-main.cc

flint_phml_CPPFLAGS = $(BOOST_CPPFLAGS) $(XML_CPPFLAGS)
flint_phml_LDFLAGS = $(BOOST_FILESYSTEM_LDFLAGS) $(BOOST_SYSTEM_LDFLAGS)
flint_phml_LDADD = libflintxx.la libflint.la libsqlite3.la $(BOOST_FILESYSTEM_LIBS) $(BOOST_SYSTEM_LIBS) $(XML_LIBS)
flint_phml_SOURCES = flint-phml-main.cc

flint_phsp_CPPFLAGS = $(BOOST_CPPFLAGS) $(XML_CPPFLAGS)
flint_phsp_LDFLAGS = $(BOOST_FILESYSTEM_LDFLAGS) $(BOOST_SYSTEM_LDFLAGS)
flint_phsp_LDADD = libflintxx.la libflint.la libsqlite3.la $(BOOST_FILESYSTEM_LIBS) $(BOOST_SYSTEM_LIBS) $(XML_LIBS)
flint_phsp_SOURCES = flint-phsp-main.cc

flint_phz_CPPFLAGS = $(BOOST_CPPFLAGS) $(LIBZIP_CFLAGS)
flint_phz_LDFLAGS = $(BOOST_FILESYSTEM_LDFLAGS) $(BOOST_SYSTEM_LDFLAGS)
flint_phz_LDADD = libflintxx.la libflint.la libsqlite3.la $(BOOST_FILESYSTEM_LIBS) $(BOOST_SYSTEM_LIBS) $(LIBZIP_LIBS)
flint_phz_SOURCES = flint-phz-main.cc

flint_run_CPPFLAGS = $(PROTOBUF_CFLAGS) $(BOOST_CPPFLAGS)
flint_run_LDFLAGS = $(BOOST_FILESYSTEM_LDFLAGS) $(BOOST_SYSTEM_LDFLAGS)
flint_run_LDADD = libflintxx.la libflint.la libsqlite3.la $(PROTOBUF_LIBS) $(BOOST_FILESYSTEM_LIBS) $(BOOST_SYSTEM_LIBS)
flint_run_SOURCES = flint-run-main.cc

flint_sbml_CPPFLAGS = $(BOOST_CPPFLAGS) $(SOSLIB_CFLAGS) $(SUNDIALS_CFLAGS) $(LIBSBML_CFLAGS)
flint_sbml_LDFLAGS = $(BOOST_FILESYSTEM_LDFLAGS) $(BOOST_SYSTEM_LDFLAGS)
flint_sbml_LDADD = libflintxx.la libflint.la libsqlite3.la $(BOOST_FILESYSTEM_LIBS) $(BOOST_SYSTEM_LIBS) $(SOSLIB_LIBS) $(SUNDIALS_LIBS) $(LIBSBML_LIBS)
flint_sbml_SOURCES = flint-sbml-main.cc

flint_sedml_CPPFLAGS = $(BOOST_CPPFLAGS) $(CLIBSEDML_CFLAGS)
flint_sedml_LDFLAGS = $(BOOST_FILESYSTEM_LDFLAGS) $(BOOST_SYSTEM_LDFLAGS)
flint_sedml_LDADD = libflintxx.la libflint.la libsqlite3.la $(BOOST_FILESYSTEM_LIBS) $(BOOST_SYSTEM_LIBS)  $(CLIBSEDML_LIBS)
flint_sedml_SOURCES = flint-sedml-main.cc

flint_spec_CPPFLAGS = $(BOOST_CPPFLAGS)
flint_spec_LDADD = libflintxx.la libflint.la libsqlite3.la
flint_spec_SOURCES = flint-spec-main.cc

flint_store_CPPFLAGS = $(PROTOBUF_CFLAGS) $(BOOST_CPPFLAGS)
flint_store_LDFLAGS = $(BOOST_PROGRAM_OPTIONS_LDFLAGS)
flint_store_LDADD = libflintxx.la libflint.la libsqlite3.la $(PROTOBUF_LIBS) $(BOOST_PROGRAM_OPTIONS_LIBS)
flint_store_SOURCES = flint-store-main.cc

flint_taskconfig_CPPFLAGS = $(AM_CFLAGS) -std=c99 -pedantic-errors
flint_taskconfig_SOURCES = flint-taskconfig-main.c

flint_taskpref_CFLAGS = $(AM_CFLAGS) -std=c99 -pedantic-errors
flint_taskpref_LDADD = libsqlite3.la
flint_taskpref_SOURCES = flint-taskpref-main.c

flint_taskspec_CFLAGS = $(AM_CFLAGS) -std=c99 -pedantic-errors
flint_taskspec_LDADD = libsqlite3.la
flint_taskspec_SOURCES = flint-taskspec-main.c

flint_timer_CPPFLAGS = $(BOOST_CPPFLAGS)
flint_timer_LDFLAGS = $(BOOST_PROGRAM_OPTIONS_LDFLAGS)
flint_timer_LDADD = $(BOOST_PROGRAM_OPTIONS_LIBS)
flint_timer_SOURCES = flint-timer-main.cc

flint_track_CPPFLAGS = $(PROTOBUF_CFLAGS) $(BOOST_CPPFLAGS)
flint_track_LDFLAGS = $(BOOST_PROGRAM_OPTIONS_LDFLAGS) $(BOOST_SYSTEM_LDFLAGS)
flint_track_LDADD = libflintxx.la libflint.la libsqlite3.la $(PROTOBUF_LIBS) $(BOOST_PROGRAM_OPTIONS_LIBS) $(BOOST_SYSTEM_LIBS)
flint_track_SOURCES = flint-track-main.cc

flint_unitoftime_CPPFLAGS = $(PROTOBUF_CFLAGS) $(BOOST_CPPFLAGS)
flint_unitoftime_LDADD = libflintxx.la libflint.la libsqlite3.la $(PROTOBUF_LIBS)
flint_unitoftime_SOURCES = flint-unitoftime-main.cc

flint_var_CPPFLAGS = $(PROTOBUF_CFLAGS) $(BOOST_CPPFLAGS)
flint_var_LDADD = libflintxx.la libflint.la libsqlite3.la $(PROTOBUF_LIBS)
flint_var_SOURCES = flint-var-main.cc

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

test_input1_CPPFLAGS = $(BOOST_CPPFLAGS)
test_input1_LDADD = libflint.la libsqlite3.la
test_input1_SOURCES = test-input1-main.cc

test_input2_CPPFLAGS = $(BOOST_CPPFLAGS)
test_input2_LDADD = libflint.la libsqlite3.la
test_input2_SOURCES = test-input2-main.cc

test_run_CPPFLAGS = $(PROTOBUF_CFLAGS) $(BOOST_CPPFLAGS)
test_run_LDADD = libflintxx.la libflint.la libsqlite3.la $(PROTOBUF_LIBS)
test_run_SOURCES = test-run-main.cc

test_singleton_CPPFLAGS = $(BOOST_CPPFLAGS)
test_singleton_LDADD = libflint.la libsqlite3.la
test_singleton_SOURCES = test-singleton-main.cc
