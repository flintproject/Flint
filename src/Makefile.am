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

AM_YFLAGS = -d -t

BUILT_SOURCES = \
    bc.pb.h \
    cli.pb.h \
    ipc.pb.h \
    lo.pb.h \
    phml.pb.h \
    unit.pb.h

bin_PROGRAMS = \
	csv2isd \
	flint-assign \
	flint-bcc \
	flint-bswap64 \
	flint-cellmlpp \
	flint-cellmltr \
	flint-cli \
	flint-combine \
	flint-combineall \
	flint-concat \
	flint-cut \
	flint-enum \
	flint-euler \
	flint-event \
	flint-evolve \
	flint-exec \
	flint-extra \
	flint-file \
	flint-filter \
	flint-flow \
	flint-form \
	flint-generate \
	flint-impl \
	flint-importdump \
	flint-importdumpall \
	flint-init \
	flint-isdh \
	flint-iv \
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
	flint-rk4 \
	flint-run \
	flint-singleton \
	flint-sbml \
	flint-sbmlall \
	flint-sbmlenc \
	flint-sedml \
	flint-sort \
	flint-spec \
	flint-store \
	flint-tac \
	flint-taskconfig \
	flint-taskpref \
	flint-taskspec \
	flint-timer \
	flint-track \
	flint-tsc \
	flint-unit \
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
	test-run

noinst_LIBRARIES = libflint.a libflintxx.a libsqlite3.a

libflint_a_CPPFLAGS = $(AM_CFLAGS) -std=c99 -pedantic-errors
libflint_a_SOURCES = database.c system.c

libflintxx_a_CPPFLAGS = $(BOOST_CPPFLAGS)
libflintxx_a_SOURCES = \
    csv/export.cc \
    modelpath.cc \
    sys/temporary_path.cc \
    utf8path.cc

libsqlite3_a_CPPFLAGS = -DSQLITE_THREADSAFE=0 -DSQLITE_OMIT_LOAD_EXTENSION
libsqlite3_a_SOURCES = sqlite3.c

csv2isd_LDADD = libflintxx.a
csv2isd_SOURCES = csv2isd-main.cc

BUILT_SOURCES += flint-assign-parser.hh
flint_assign_CPPFLAGS = $(BOOST_CPPFLAGS)
flint_assign_SOURCES = flint-assign-parser.yy flint-assign-lexer.ll

BUILT_SOURCES += flint-bcc-parser.hh
flint_bcc_CPPFLAGS = $(PROTOBUF_CFLAGS) $(BOOST_CPPFLAGS)
flint_bcc_LDADD = $(PROTOBUF_LIBS)
flint_bcc_SOURCES = flint-bcc-block.cc flint-bcc-parser.yy flint-bcc-lexer.ll bc.pb.cc

flint_bswap64_SOURCES = flint-bswap64-main.c

flint_cellmlpp_CPPFLAGS = $(BOOST_CPPFLAGS) $(XML_CPPFLAGS)
flint_cellmlpp_LDFLAGS = $(BOOST_FILESYSTEM_LDFLAGS) $(BOOST_SYSTEM_LDFLAGS)
flint_cellmlpp_LDADD = libflintxx.a libflint.a libsqlite3.a $(BOOST_FILESYSTEM_LIBS) $(BOOST_SYSTEM_LIBS) $(XML_LIBS)
flint_cellmlpp_SOURCES = flint-cellmlpp-main.cc

flint_cellmltr_CPPFLAGS = $(BOOST_CPPFLAGS)
flint_cellmltr_LDFLAGS = $(BOOST_FILESYSTEM_LDFLAGS) $(BOOST_SYSTEM_LDFLAGS)
flint_cellmltr_LDADD = libflintxx.a libflint.a libsqlite3.a $(BOOST_FILESYSTEM_LIBS) $(BOOST_SYSTEM_LIBS)
flint_cellmltr_SOURCES = uuidgen.cc flint-cellmltr-main.cc

flint_cli_CPPFLAGS = $(PROTOBUF_CFLAGS) $(BOOST_CPPFLAGS)
flint_cli_LDFLAGS = $(BOOST_PROGRAM_OPTIONS_LDFLAGS)
flint_cli_LDADD = $(PROTOBUF_LIBS) $(BOOST_PROGRAM_OPTIONS_LIBS)
flint_cli_SOURCES = cli.pb.cc flint-cli-main.cc

BUILT_SOURCES += flint-combine-parser.hh
flint_combine_CPPFLAGS = $(BOOST_CPPFLAGS)
flint_combine_LDFLAGS = $(BOOST_PROGRAM_OPTIONS_LDFLAGS)
flint_combine_LDADD = libsqlite3.a $(BOOST_PROGRAM_OPTIONS_LIBS)
flint_combine_SOURCES = flint-combine-parser.yy flint-combine-lexer.ll

flint_combineall_CPPFLAGS = $(BOOST_CPPFLAGS)
flint_combineall_LDFLAGS = $(BOOST_PROGRAM_OPTIONS_LDFLAGS)
flint_combineall_LDADD = libflint.a $(BOOST_PROGRAM_OPTIONS_LIBS)
flint_combineall_SOURCES = flint-combineall-main.cc

flint_concat_CFLAGS = $(AM_CFLAGS) -std=c99 -pedantic-errors
flint_concat_SOURCES = flint-concat-main.c

flint_cut_CPPFLAGS = $(PROTOBUF_CFLAGS) $(BOOST_CPPFLAGS)
flint_cut_LDFLAGS = $(BOOST_PROGRAM_OPTIONS_LDFLAGS) $(BOOST_SYSTEM_LDFLAGS)
flint_cut_LDADD = $(PROTOBUF_LIBS) $(BOOST_PROGRAM_OPTIONS_LIBS) $(BOOST_SYSTEM_LIBS)
flint_cut_SOURCES = lo.pb.cc flint-cut-main.cc

flint_enum_CFLAGS = $(AM_CFLAGS) -std=c99 -pedantic-errors
flint_enum_LDADD = libsqlite3.a
flint_enum_SOURCES = flint-enum-main.c

BUILT_SOURCES += flint-euler-parser.hh
flint_euler_CPPFLAGS = $(BOOST_CPPFLAGS)
flint_euler_SOURCES = flint-euler-parser.yy flint-euler-lexer.ll

BUILT_SOURCES += flint-event-parser.hh
flint_event_CPPFLAGS = $(BOOST_CPPFLAGS)
flint_event_SOURCES = flint-event-parser.yy flint-event-lexer.ll

flint_evolve_CPPFLAGS = $(PROTOBUF_CFLAGS) $(BOOST_CPPFLAGS)
flint_evolve_LDFLAGS = $(BOOST_PROGRAM_OPTIONS_LDFLAGS)
flint_evolve_LDADD = $(PROTOBUF_LIBS) $(BOOST_PROGRAM_OPTIONS_LIBS)
flint_evolve_SOURCES = bc.pb.cc lo.pb.cc flint-evolve-main.cc

flint_exec_CPPFLAGS = $(BOOST_CPPFLAGS)
flint_exec_LDFLAGS = $(BOOST_FILESYSTEM_LDFLAGS) $(BOOST_SYSTEM_LDFLAGS)
flint_exec_LDADD = libflint.a libsqlite3.a $(BOOST_FILESYSTEM_LIBS) $(BOOST_SYSTEM_LIBS)
flint_exec_SOURCES = flint-exec-main.cc

flint_extra_CPPFLAGS = $(BOOST_CPPFLAGS)
flint_extra_LDFLAGS = $(BOOST_PROGRAM_OPTIONS_LDFLAGS)
flint_extra_LDADD = libsqlite3.a $(BOOST_PROGRAM_OPTIONS_LIBS)
flint_extra_SOURCES = flint-extra-main.cc

flint_file_CPPFLAGS = $(BOOST_CPPFLAGS) $(XML_CPPFLAGS)
flint_file_LDFLAGS = $(BOOST_FILESYSTEM_LDFLAGS) $(BOOST_SYSTEM_LDFLAGS) $(BOOST_PROGRAM_OPTIONS_LDFLAGS)
flint_file_LDADD = libflintxx.a libflint.a libsqlite3.a $(BOOST_FILESYSTEM_LIBS) $(BOOST_SYSTEM_LIBS) $(BOOST_PROGRAM_OPTIONS_LIBS) $(XML_LIBS)
flint_file_SOURCES = flint-file-main.cc

flint_filter_CPPFLAGS = $(PROTOBUF_CFLAGS) $(BOOST_CPPFLAGS)
flint_filter_LDFLAGS = $(BOOST_PROGRAM_OPTIONS_LDFLAGS) $(BOOST_SYSTEM_LDFLAGS)
flint_filter_LDADD = libsqlite3.a $(PROTOBUF_LIBS) $(BOOST_PROGRAM_OPTIONS_LIBS) $(BOOST_SYSTEM_LIBS)
flint_filter_SOURCES = lo.pb.cc flint-filter-main.cc

flint_flow_CPPFLAGS = $(PROTOBUF_CFLAGS) $(BOOST_CPPFLAGS)
flint_flow_LDADD = libsqlite3.a $(PROTOBUF_LIBS)
flint_flow_SOURCES = lo.pb.cc flint-flow-main.cc

flint_form_CFLAGS = $(AM_CFLAGS) -std=c99 -pedantic-errors
flint_form_LDADD = libsqlite3.a
flint_form_SOURCES = flint-form-main.c

flint_generate_CFLAGS = $(AM_CFLAGS) -std=c99 -pedantic-errors
flint_generate_LDADD = libsqlite3.a
flint_generate_SOURCES = flint-generate-main.c

flint_impl_CPPFLAGS = $(BOOST_CPPFLAGS)
flint_impl_LDFLAGS = $(BOOST_PROGRAM_OPTIONS_LDFLAGS)
flint_impl_LDADD = libsqlite3.a $(BOOST_PROGRAM_OPTIONS_LIBS)
flint_impl_SOURCES = flint-impl-main.cc

flint_importdump_CPPFLAGS = $(BOOST_CPPFLAGS) $(XML_CPPFLAGS)
flint_importdump_LDFLAGS = $(BOOST_FILESYSTEM_LDFLAGS) $(BOOST_PROGRAM_OPTIONS_LDFLAGS) $(BOOST_SYSTEM_LDFLAGS)
flint_importdump_LDADD = libflintxx.a libflint.a libsqlite3.a $(BOOST_FILESYSTEM_LIBS) $(BOOST_PROGRAM_OPTIONS_LIBS) $(BOOST_SYSTEM_LIBS) $(XML_LIBS)
flint_importdump_SOURCES = flint-importdump-main.cc

flint_importdumpall_CPPFLAGS = $(BOOST_CPPFLAGS)
flint_importdumpall_LDFLAGS = $(BOOST_FILESYSTEM_LDFLAGS) $(BOOST_SYSTEM_LDFLAGS)
flint_importdumpall_LDADD = libflintxx.a libflint.a libsqlite3.a $(BOOST_FILESYSTEM_LIBS) $(BOOST_SYSTEM_LIBS)
flint_importdumpall_SOURCES = flint-importdumpall-main.cc

flint_init_CPPFLAGS = $(PROTOBUF_CFLAGS) $(BOOST_CPPFLAGS)
flint_init_LDFLAGS = $(BOOST_PROGRAM_OPTIONS_LDFLAGS)
flint_init_LDADD = libsqlite3.a $(PROTOBUF_LIBS) $(BOOST_PROGRAM_OPTIONS_LIBS)
flint_init_SOURCES = bc.pb.cc lo.pb.cc flint-init-main.cc

flint_isdh_CPPFLAGS = $(PROTOBUF_CFLAGS) $(BOOST_CPPFLAGS)
flint_isdh_LDFLAGS = $(BOOST_PROGRAM_OPTIONS_LDFLAGS)
flint_isdh_LDADD = $(PROTOBUF_LIBS) $(BOOST_PROGRAM_OPTIONS_LIBS)
flint_isdh_SOURCES = lo.pb.cc flint-isdh-main.cc

flint_iv_CPPFLAGS = $(BOOST_CPPFLAGS)
flint_iv_LDFLAGS = $(BOOST_PROGRAM_OPTIONS_LDFLAGS)
flint_iv_LDADD = libsqlite3.a $(BOOST_PROGRAM_OPTIONS_LIBS)
flint_iv_SOURCES = flint-iv-main.cc

flint_lands_CPPFLAGS = $(PROTOBUF_CFLAGS) $(BOOST_CPPFLAGS)
flint_lands_LDFLAGS = $(BOOST_PROGRAM_OPTIONS_LDFLAGS)
flint_lands_LDADD = $(PROTOBUF_LIBS) $(BOOST_PROGRAM_OPTIONS_LIBS)
flint_lands_SOURCES = ipc.pb.cc phml.pb.cc flint-lands-main.cc

flint_layout_CPPFLAGS = $(PROTOBUF_CFLAGS) $(BOOST_CPPFLAGS)
flint_layout_LDFLAGS = $(BOOST_PROGRAM_OPTIONS_LDFLAGS)
flint_layout_LDADD = libsqlite3.a $(PROTOBUF_LIBS) $(BOOST_PROGRAM_OPTIONS_LIBS)
flint_layout_SOURCES = flint-layout-main.cc lo.pb.cc

flint_loadconfig_CPPFLAGS = $(AM_CFLAGS) -std=c99 -pedantic-errors
flint_loadconfig_SOURCES = flint-loadconfig-main.c

flint_lodbg_CPPFLAGS = $(PROTOBUF_CFLAGS) $(BOOST_CPPFLAGS)
flint_lodbg_LDADD = $(PROTOBUF_LIBS)
flint_lodbg_SOURCES = flint-lodbg-main.cc lo.pb.cc

flint_nc_CPPFLAGS = $(PROTOBUF_CFLAGS) $(BOOST_CPPFLAGS)
flint_nc_LDADD = libsqlite3.a $(PROTOBUF_LIBS)
flint_nc_SOURCES = phml.pb.cc flint-nc-main.cc

flint_open_CPPFLAGS = $(BOOST_CPPFLAGS)
flint_open_LDFLAGS = $(BOOST_FILESYSTEM_LDFLAGS) $(BOOST_SYSTEM_LDFLAGS)
flint_open_LDADD = libflint.a libsqlite3.a $(BOOST_FILESYSTEM_LIBS) $(BOOST_SYSTEM_LIBS)
flint_open_SOURCES = flint-open-main.cc

flint_param_CPPFLAGS = $(PROTOBUF_CFLAGS) $(BOOST_CPPFLAGS)
flint_param_LDFLAGS = $(BOOST_PROGRAM_OPTIONS_LDFLAGS) $(BOOST_SYSTEM_LDFLAGS)
flint_param_LDADD = $(PROTOBUF_LIBS) $(BOOST_PROGRAM_OPTIONS_LIBS) $(BOOST_SYSTEM_LIBS)
flint_param_SOURCES = lo.pb.cc flint-param-main.cc

flint_phml_CPPFLAGS = $(BOOST_CPPFLAGS) $(XML_CPPFLAGS)
flint_phml_LDFLAGS = $(BOOST_FILESYSTEM_LDFLAGS) $(BOOST_SYSTEM_LDFLAGS)
flint_phml_LDADD = libflintxx.a libflint.a libsqlite3.a $(BOOST_FILESYSTEM_LIBS) $(BOOST_SYSTEM_LIBS) $(XML_LIBS)
flint_phml_SOURCES = \
    branch.cc \
    phml/graph-iv-rewriter.cc \
    phml/graph-math-rewriter.cc \
    phml/transition-form.cc \
    reach.cc \
    span.cc \
    sprinkle.cc \
    uuidgen.cc \
    flint-phml-main.cc

flint_phsp_CPPFLAGS = $(BOOST_CPPFLAGS) $(XML_CPPFLAGS)
flint_phsp_LDFLAGS = $(BOOST_FILESYSTEM_LDFLAGS) $(BOOST_SYSTEM_LDFLAGS)
flint_phsp_LDADD = libflintxx.a libflint.a libsqlite3.a $(BOOST_FILESYSTEM_LIBS) $(BOOST_SYSTEM_LIBS) $(XML_LIBS)
flint_phsp_SOURCES = flint-phsp-main.cc

flint_phz_CPPFLAGS = $(BOOST_CPPFLAGS) $(LIBZIP_CFLAGS)
flint_phz_LDFLAGS = $(BOOST_FILESYSTEM_LDFLAGS) $(BOOST_SYSTEM_LDFLAGS)
flint_phz_LDADD = libflintxx.a libflint.a libsqlite3.a $(BOOST_FILESYSTEM_LIBS) $(BOOST_SYSTEM_LIBS) $(LIBZIP_LIBS)
flint_phz_SOURCES = flint-phz-main.cc

BUILT_SOURCES += flint-rk4-parser.hh
flint_rk4_CPPFLAGS = $(BOOST_CPPFLAGS)
flint_rk4_SOURCES = flint-rk4-parser.yy flint-rk4-lexer.ll

flint_run_CPPFLAGS = $(PROTOBUF_CFLAGS) $(BOOST_CPPFLAGS)
flint_run_LDFLAGS = $(BOOST_FILESYSTEM_LDFLAGS) $(BOOST_SYSTEM_LDFLAGS)
flint_run_LDADD = libflintxx.a libflint.a libsqlite3.a $(PROTOBUF_LIBS) $(BOOST_FILESYSTEM_LIBS) $(BOOST_SYSTEM_LIBS)
flint_run_SOURCES = flint-run-main.cc cli.pb.cc

flint_sbml_CPPFLAGS = $(BOOST_CPPFLAGS) $(SOSLIB_CFLAGS) $(SUNDIALS_CFLAGS) $(LIBSBML_CFLAGS)
flint_sbml_LDFLAGS = $(BOOST_FILESYSTEM_LDFLAGS) $(BOOST_PROGRAM_OPTIONS_LDFLAGS) $(BOOST_SYSTEM_LDFLAGS)
flint_sbml_LDADD = libflintxx.a libflint.a libsqlite3.a $(BOOST_FILESYSTEM_LIBS) $(BOOST_PROGRAM_OPTIONS_LIBS) $(BOOST_SYSTEM_LIBS) $(SOSLIB_LIBS) $(SUNDIALS_LIBS) $(LIBSBML_LIBS)
flint_sbml_SOURCES = flint-sbml-main.cc

flint_sbmlall_CPPFLAGS = $(BOOST_CPPFLAGS)
flint_sbmlall_LDFLAGS = $(BOOST_FILESYSTEM_LDFLAGS) $(BOOST_PROGRAM_OPTIONS_LDFLAGS) $(BOOST_SYSTEM_LDFLAGS)
flint_sbmlall_LDADD = libflint.a $(BOOST_FILESYSTEM_LIBS) $(BOOST_PROGRAM_OPTIONS_LIBS) $(BOOST_SYSTEM_LIBS)
flint_sbmlall_SOURCES = flint-sbmlall-main.cc

flint_sbmlenc_CPPFLAGS = $(BOOST_CPPFLAGS)
flint_sbmlenc_LDFLAGS = $(BOOST_PROGRAM_OPTIONS_LDFLAGS)
flint_sbmlenc_LDADD = $(BOOST_PROGRAM_OPTIONS_LIBS)
flint_sbmlenc_SOURCES = flint-sbmlenc-main.cc

flint_sedml_CPPFLAGS = $(BOOST_CPPFLAGS) $(CLIBSEDML_CFLAGS)
flint_sedml_LDFLAGS = $(BOOST_FILESYSTEM_LDFLAGS) $(BOOST_SYSTEM_LDFLAGS)
flint_sedml_LDADD = libflintxx.a libflint.a libsqlite3.a $(BOOST_FILESYSTEM_LIBS) $(BOOST_SYSTEM_LIBS)  $(CLIBSEDML_LIBS)
flint_sedml_SOURCES = flint-sedml-main.cc

flint_singleton_CPPFLAGS = $(BOOST_CPPFLAGS)
flint_singleton_LDADD = libsqlite3.a
flint_singleton_SOURCES = flint-singleton-main.cc

BUILT_SOURCES += flint-sort-parser.hh
flint_sort_CPPFLAGS = $(BOOST_CPPFLAGS)
flint_sort_SOURCES = flint-sort-parser.yy flint-sort-lexer.ll

flint_spec_CPPFLAGS = $(PROTOBUF_CFLAGS) $(BOOST_CPPFLAGS)
flint_spec_LDFLAGS = $(BOOST_PROGRAM_OPTIONS_LDFLAGS)
flint_spec_LDADD = $(PROTOBUF_LIBS) $(BOOST_PROGRAM_OPTIONS_LIBS)
flint_spec_SOURCES = lo.pb.cc flint-spec-main.cc

flint_store_CPPFLAGS = $(PROTOBUF_CFLAGS) $(BOOST_CPPFLAGS)
flint_store_LDFLAGS = $(BOOST_PROGRAM_OPTIONS_LDFLAGS)
flint_store_LDADD = libsqlite3.a $(PROTOBUF_LIBS) $(BOOST_PROGRAM_OPTIONS_LIBS)
flint_store_SOURCES = lo.pb.cc flint-store-main.cc

BUILT_SOURCES += flint-tac-parser.hh
flint_tac_CPPFLAGS = $(BOOST_CPPFLAGS)
flint_tac_SOURCES = flint-tac-parser.yy flint-tac-lexer.ll

flint_taskconfig_CPPFLAGS = $(AM_CFLAGS) -std=c99 -pedantic-errors
flint_taskconfig_SOURCES = flint-taskconfig-main.c

flint_taskpref_CFLAGS = $(AM_CFLAGS) -std=c99 -pedantic-errors
flint_taskpref_LDADD = libsqlite3.a
flint_taskpref_SOURCES = flint-taskpref-main.c

flint_taskspec_CFLAGS = $(AM_CFLAGS) -std=c99 -pedantic-errors
flint_taskspec_LDADD = libsqlite3.a
flint_taskspec_SOURCES = flint-taskspec-main.c

flint_timer_CPPFLAGS = $(BOOST_CPPFLAGS)
flint_timer_LDFLAGS = $(BOOST_PROGRAM_OPTIONS_LDFLAGS)
flint_timer_LDADD = $(BOOST_PROGRAM_OPTIONS_LIBS)
flint_timer_SOURCES = flint-timer-main.cc

flint_track_CPPFLAGS = $(PROTOBUF_CFLAGS) $(BOOST_CPPFLAGS)
flint_track_LDFLAGS = $(BOOST_PROGRAM_OPTIONS_LDFLAGS) $(BOOST_SYSTEM_LDFLAGS)
flint_track_LDADD = $(PROTOBUF_LIBS) $(BOOST_PROGRAM_OPTIONS_LIBS) $(BOOST_SYSTEM_LIBS)
flint_track_SOURCES = lo.pb.cc ipc.pb.cc flint-track-main.cc

flint_tsc_CPPFLAGS = $(BOOST_CPPFLAGS)
flint_tsc_LDFLAGS = $(BOOST_FILESYSTEM_LDFLAGS) $(BOOST_SYSTEM_LDFLAGS)
flint_tsc_LDADD = libflintxx.a libsqlite3.a $(BOOST_FILESYSTEM_LIBS) $(BOOST_SYSTEM_LIBS)
flint_tsc_SOURCES = flint-tsc-main.cc

flint_unit_CPPFLAGS = $(PROTOBUF_CFLAGS) $(BOOST_CPPFLAGS)
flint_unit_LDADD = libsqlite3.a $(PROTOBUF_LIBS)
flint_unit_SOURCES = unit.pb.cc flint-unit-main.cc

flint_unitoftime_CPPFLAGS = $(PROTOBUF_CFLAGS) $(BOOST_CPPFLAGS)
flint_unitoftime_LDFLAGS = $(BOOST_PROGRAM_OPTIONS_LDFLAGS)
flint_unitoftime_LDADD = $(PROTOBUF_LIBS) $(BOOST_PROGRAM_OPTIONS_LIBS)
flint_unitoftime_SOURCES = ipc.pb.cc unit.pb.cc flint-unitoftime-main.cc

flint_var_CPPFLAGS = $(PROTOBUF_CFLAGS) $(BOOST_CPPFLAGS)
flint_var_LDFLAGS = $(BOOST_PROGRAM_OPTIONS_LDFLAGS) $(BOOST_SYSTEM_LDFLAGS)
flint_var_LDADD = $(PROTOBUF_LIBS) $(BOOST_PROGRAM_OPTIONS_LIBS) $(BOOST_SYSTEM_LIBS)
flint_var_SOURCES = lo.pb.cc flint-var-main.cc

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
isdplot_LDADD = libflintxx.a libflint.a $(BOOST_FILESYSTEM_LIBS) $(BOOST_SYSTEM_LIBS) $(BOOST_PROGRAM_OPTIONS_LIBS)
isdplot_SOURCES = isdplot-main.cc

isdrand_CFLAGS = $(AM_CFLAGS) -std=c99 -pedantic-errors
isdrand_SOURCES = isdrand-main.c

isdsort_CPPFLAGS = $(BOOST_CPPFLAGS)
isdsort_SOURCES = isdsort-main.cc

isdstrip_CPPFLAGS = $(BOOST_CPPFLAGS)
isdstrip_LDFLAGS = $(BOOST_FILESYSTEM_LDFLAGS) $(BOOST_SYSTEM_LDFLAGS) $(BOOST_PROGRAM_OPTIONS_LDFLAGS)
isdstrip_LDADD = libflintxx.a $(BOOST_FILESYSTEM_LIBS) $(BOOST_SYSTEM_LIBS) $(BOOST_PROGRAM_OPTIONS_LIBS)
isdstrip_SOURCES = isdstrip-main.cc

isdwc_CPPFLAGS = $(BOOST_CPPFLAGS)
isdwc_LDFLAGS = $(BOOST_PROGRAM_OPTIONS_LDFLAGS)
isdwc_LDADD = $(BOOST_PROGRAM_OPTIONS_LIBS)
isdwc_SOURCES = isdwc-main.cc

test_input1_LDADD = libflint.a libsqlite3.a
test_input1_SOURCES = test-input1-main.c

test_input2_LDADD = libflint.a libsqlite3.a
test_input2_SOURCES = test-input2-main.c

test_run_CPPFLAGS = $(PROTOBUF_CFLAGS) $(BOOST_CPPFLAGS)
test_run_LDADD = $(PROTOBUF_LIBS)
test_run_SOURCES = test-run-main.cc cli.pb.cc
