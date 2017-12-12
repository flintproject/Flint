# -*- Mode: makefile-gmake; tab-width: 4; indent-tabs-mode: t -*-
-include config.mk
include ../deposit.mk
include ../option.mk

UNNECESSARY_LIBRARIES = libxml2-%
EXTERNAL_LIBRARIES = $(filter-out $(UNNECESSARY_LIBRARIES),$(ALL_EXTERNAL_LIBRARIES))
EXTERNAL_LIBRARY_DIRS = $(filter-out $(UNNECESSARY_LIBRARIES),$(ALL_EXTERNAL_LIBRARY_DIRS))

PREFIX ?= $(abspath opt)

PKG_CONFIG_PATH = $(PREFIX)/clibsedml-$(CLIBSEDML_VERSION)/lib/pkgconfig:$(PREFIX)/czmq-$(CZMQ_VERSION)/lib/pkgconfig:$(PREFIX)/libmicrohttpd-$(LIBMICROHTTPD_VERSION)/lib/pkgconfig:$(PREFIX)/libsbml-$(LIBSBML_VERSION)/lib/pkgconfig:$(PREFIX)/protobuf-$(PROTOBUF_VERSION)/lib/pkgconfig:$(PREFIX)/SBML_odeSolver-$(SOSLIB_VERSION)/lib/pkgconfig:$(PREFIX)/zeromq-$(ZEROMQ_VERSION)/lib/pkgconfig

.PHONY: all check check-sequential-version download external mostlyclean clean distclean install

all: download tmp/sequential-version.installed

check: check-sequential-version

check-sequential-version: tmp/sequential-version.installed
	$(MAKE) -C var/sequential-version check

download: $(foreach lib,$(EXTERNAL_LIBRARIES),src/$(lib))

external: tmp/$(BOOST_UNDERSCORE).installed tmp/ceres-solver-$(CERES_VERSION).installed tmp/clibsedml-$(CLIBSEDML_VERSION).installed tmp/czmq-$(CZMQ_VERSION).installed tmp/libmicrohttpd-$(LIBMICROHTTPD_VERSION).installed tmp/wxWidgets-$(WXWIDGETS_VERSION).installed tmp/protobuf-$(PROTOBUF_VERSION).installed tmp/SBML_odeSolver-$(SOSLIB_VERSION).installed

mostlyclean:
	-rm -rf tmp/sequential-version.installed opt/sequential-version var/sequential-version

clean: mostlyclean
	-rm -rf tmp/*.installed opt $(foreach lib,$(EXTERNAL_LIBRARY_DIRS),tmp/$(lib) var/$(lib))

distclean: clean
	-rm -rf $(foreach lib,$(EXTERNAL_LIBRARIES),src/$(lib))

var:
	install -d $@

tmp: | var
	install -d $@

tmp/$(BOOST_UNDERSCORE): src/$(BOOST_UNDERSCORE).tar.bz2 | tmp
	-rm -rf var/$(BOOST_UNDERSCORE)
	tar -C var -jxf $<
	touch $@

tmp/ceres-solver-$(CERES_VERSION): src/ceres-solver-$(CERES_VERSION).tar.gz | tmp
	-rm -rf src/ceres-solver-$(CERES_VERSION)
	tar -C src -zxf $<
	touch $@

tmp/clibsedml-$(CLIBSEDML_VERSION): src/clibsedml-$(CLIBSEDML_VERSION).tar.gz | tmp
	-rm -rf var/clibsedml-$(CLIBSEDML_VERSION)
	tar -C var -zxf $<
	touch $@

tmp/czmq-$(CZMQ_VERSION): src/czmq-$(CZMQ_VERSION).tar.gz czmq-$(CZMQ_VERSION)-0.patch czmq-$(CZMQ_VERSION)-1.patch | tmp
	-rm -rf var/czmq-$(CZMQ_VERSION)
	tar -C var -zxf $<
	patch var/czmq-$(CZMQ_VERSION)/src/zgossip_engine.inc czmq-$(CZMQ_VERSION)-0.patch
	patch var/czmq-$(CZMQ_VERSION)/src/zsys.c czmq-$(CZMQ_VERSION)-1.patch
	touch $@

tmp/libmicrohttpd-$(LIBMICROHTTPD_VERSION): src/libmicrohttpd-$(LIBMICROHTTPD_VERSION).tar.gz | tmp
	-rm -rf var/libmicrohttpd-$(LIBMICROHTTPD_VERSION)
	tar -C var -zxf $<
	touch $@

tmp/libsbml-$(LIBSBML_VERSION): src/libSBML-$(LIBSBML_VERSION)-core-src.zip | tmp
	-rm -rf var/libsbml-$(LIBSBML_VERSION)
	(cd var && unzip -qq ../$<)
	touch $@

tmp/protobuf-$(PROTOBUF_VERSION): src/protobuf-cpp-$(PROTOBUF_VERSION).tar.gz | tmp
	-rm -rf var/protobuf-$(PROTOBUF_VERSION)
	tar -C var -zxf $<
	touch $@

tmp/SBML_odeSolver-$(SOSLIB_VERSION): src/SBML_odeSolver-$(SOSLIB_VERSION).tar.gz | tmp
	-rm -rf var/SBML_odeSolver-$(SOSLIB_VERSION)
	tar -C var -zxf $<
	touch $@

tmp/sundials-$(SUNDIALS_VERSION): src/sundials-$(SUNDIALS_VERSION).tar.gz | tmp
	-rm -rf src/sundials-$(SUNDIALS_VERSION)
	tar -C src -zxf $<
	touch $@

tmp/wxWidgets-$(WXWIDGETS_VERSION): src/wxWidgets-$(WXWIDGETS_VERSION).tar.bz2 | tmp
	-rm -rf var/wxWidgets-$(WXWIDGETS_VERSION)
	tar -C var -jxf $<
	touch $@

tmp/zeromq-$(ZEROMQ_VERSION): src/zeromq-$(ZEROMQ_VERSION).tar.gz | tmp
	-rm -rf var/zeromq-$(ZEROMQ_VERSION)
	tar -C var -zxf $<
	touch $@

tmp/$(BOOST_UNDERSCORE).installed: tmp/$(BOOST_UNDERSCORE)
	(cd var/$(BOOST_UNDERSCORE) && ./bootstrap.sh --prefix=$(PREFIX)/$(BOOST_UNDERSCORE) --without-icu && ./b2 $(B2_WITHOUT_OPTIONS) --layout=tagged link=shared install)
	touch $@

tmp/ceres-solver-$(CERES_VERSION).installed: tmp/ceres-solver-$(CERES_VERSION)
	-rm -rf var/ceres-solver-$(CERES_VERSION)
	install -d var/ceres-solver-$(CERES_VERSION)
	(cd var/ceres-solver-$(CERES_VERSION) && cmake -DCMAKE_INSTALL_PREFIX=$(PREFIX)/ceres-solver-$(CERES_VERSION) -DEIGEN_INCLUDE_DIR=$(abspath ../include) $(CERES_CMAKE_OPTIONS) ../../src/ceres-solver-$(CERES_VERSION))
	$(MAKE) -C var/ceres-solver-$(CERES_VERSION) all
	-rm -rf $(PREFIX)/ceres-solver-$(CERES_VERSION)
	$(MAKE) -C var/ceres-solver-$(CERES_VERSION) install
	touch $@

tmp/clibsedml-$(CLIBSEDML_VERSION).installed: tmp/clibsedml-$(CLIBSEDML_VERSION)
	(cd var/clibsedml-$(CLIBSEDML_VERSION) && autoreconf -i -f && ./configure --prefix=$(PREFIX)/clibsedml-$(CLIBSEDML_VERSION) $(CLIBSEDML_CONFIGURE_OPTIONS))
	$(MAKE) -C var/clibsedml-$(CLIBSEDML_VERSION) all
	-rm -rf $(PREFIX)/clibsedml-$(CLIBSEDML_VERSION)
	$(MAKE) -C var/clibsedml-$(CLIBSEDML_VERSION) install
	touch $@

tmp/czmq-$(CZMQ_VERSION).installed: tmp/czmq-$(CZMQ_VERSION) tmp/zeromq-$(ZEROMQ_VERSION).installed
	(cd var/czmq-$(CZMQ_VERSION) && ./configure --prefix=$(PREFIX)/czmq-$(CZMQ_VERSION) $(CZMQ_CONFIGURE_OPTIONS) PKG_CONFIG_PATH="$(PREFIX)/zeromq-$(ZEROMQ_VERSION)/lib/pkgconfig")
	$(MAKE) -C var/czmq-$(CZMQ_VERSION) all
	-rm -rf $(PREFIX)/czmq-$(CZMQ_VERSION)
	$(MAKE) -C var/czmq-$(CZMQ_VERSION) install
	touch $@

tmp/libmicrohttpd-$(LIBMICROHTTPD_VERSION).installed: tmp/libmicrohttpd-$(LIBMICROHTTPD_VERSION)
	(cd var/libmicrohttpd-$(LIBMICROHTTPD_VERSION) && ./configure --prefix=$(PREFIX)/libmicrohttpd-$(LIBMICROHTTPD_VERSION) $(LIBMICROHTTPD_CONFIGURE_OPTIONS))
	$(MAKE) -C var/libmicrohttpd-$(LIBMICROHTTPD_VERSION) all
	-rm -rf $(PREFIX)/libmicrohttpd-$(LIBMICROHTTPD_VERSION)
	$(MAKE) -C var/libmicrohttpd-$(LIBMICROHTTPD_VERSION) install
	touch $@

tmp/protobuf-$(PROTOBUF_VERSION).installed: tmp/protobuf-$(PROTOBUF_VERSION)
	(cd var/protobuf-$(PROTOBUF_VERSION) && ./configure --prefix=$(PREFIX)/protobuf-$(PROTOBUF_VERSION) $(PROTOBUF_CONFIGURE_OPTONS))
	$(MAKE) -C var/protobuf-$(PROTOBUF_VERSION) all
	-rm -rf $(PREFIX)/protobuf-$(PROTOBUF_VERSION)
	$(MAKE) -C var/protobuf-$(PROTOBUF_VERSION) install
	touch $@

tmp/sundials-$(SUNDIALS_VERSION).installed: tmp/sundials-$(SUNDIALS_VERSION)
	-rm -rf var/sundials-$(SUNDIALS_VERSION)
	install -d var/sundials-$(SUNDIALS_VERSION)
	(cd var/sundials-$(SUNDIALS_VERSION) && cmake -DCMAKE_INSTALL_PREFIX=$(PREFIX)/sundials-$(SUNDIALS_VERSION) $(SUNDIALS_CMAKE_OPTIONS) ../../src/sundials-$(SUNDIALS_VERSION))
	$(MAKE) -C var/sundials-$(SUNDIALS_VERSION) all
	-rm -rf $(PREFIX)/sundials-$(SUNDIALS_VERSION)
	$(MAKE) -C var/sundials-$(SUNDIALS_VERSION) install
	touch $@

tmp/wxWidgets-$(WXWIDGETS_VERSION).installed: tmp/wxWidgets-$(WXWIDGETS_VERSION)
	(cd var/wxWidgets-$(WXWIDGETS_VERSION) && ./configure --prefix=$(PREFIX)/wxWidgets-$(WXWIDGETS_VERSION) $(WXWIDGETS_CONFIGURE_OPTIONS))
	$(MAKE) -C var/wxWidgets-$(WXWIDGETS_VERSION) all
	-rm -rf $(PREFIX)/wxWidgets-$(WXWIDGETS_VERSION)
	$(MAKE) -C var/wxWidgets-$(WXWIDGETS_VERSION) install
	touch $@

tmp/libsbml-$(LIBSBML_VERSION).installed: tmp/libsbml-$(LIBSBML_VERSION)
	(cd var/libsbml-$(LIBSBML_VERSION) && ./configure --prefix=$(PREFIX)/libsbml-$(LIBSBML_VERSION) --with-libxml $(LIBSBML_CONFIGURE_OPTIONS))
	$(MAKE) -C var/libsbml-$(LIBSBML_VERSION) all
	-rm -rf $(PREFIX)/libsbml-$(LIBSBML_VERSION)
	$(MAKE) -C var/libsbml-$(LIBSBML_VERSION) install
	touch $@

tmp/SBML_odeSolver-$(SOSLIB_VERSION).installed: tmp/SBML_odeSolver-$(SOSLIB_VERSION) tmp/sundials-$(SUNDIALS_VERSION).installed tmp/libsbml-$(LIBSBML_VERSION).installed
	(cd var/SBML_odeSolver-$(SOSLIB_VERSION) && autoreconf -i && ./configure --prefix=$(PREFIX)/SBML_odeSolver-$(SOSLIB_VERSION) --with-libsbml=$(PREFIX)/libsbml-$(LIBSBML_VERSION) --with-libxml2 --with-sundials=$(PREFIX)/sundials-$(SUNDIALS_VERSION) $(SOSLIB_CONFIGURE_OPTIONS))
	$(MAKE) -C var/SBML_odeSolver-$(SOSLIB_VERSION) all
	-rm -rf $(PREFIX)/SBML_odeSolver-$(SOSLIB_VERSION)
	$(MAKE) -C var/SBML_odeSolver-$(SOSLIB_VERSION) install
	touch $@

tmp/zeromq-$(ZEROMQ_VERSION).installed: tmp/zeromq-$(ZEROMQ_VERSION)
	(cd var/zeromq-$(ZEROMQ_VERSION) && ./configure --prefix=$(PREFIX)/zeromq-$(ZEROMQ_VERSION) $(ZEROMQ_CONFIGURE_OPTIONS))
	$(MAKE) -C var/zeromq-$(ZEROMQ_VERSION) all
	-rm -rf $(PREFIX)/zeromq-$(ZEROMQ_VERSION)
	$(MAKE) -C var/zeromq-$(ZEROMQ_VERSION) install
	touch $@

tmp/sequential-version.installed: external
	(cd .. && autoreconf -i -f)
	install -d var/sequential-version
	(cd var/sequential-version && env LDFLAGS='-Wl,-rpath -Wl,$(PREFIX)/libsbml-$(LIBSBML_VERSION)/lib -Wl,-rpath -Wl,$(PREFIX)/sundials-$(SUNDIALS_VERSION)/lib -Wl,-rpath -Wl,$(PREFIX)/wxWidgets-$(WXWIDGETS_VERSION)/lib' PKG_CONFIG_PATH=$(PKG_CONFIG_PATH) ../../../configure --prefix=$(PREFIX)/sequential-version --with-boost=$(PREFIX)/$(BOOST_UNDERSCORE) --with-ceres-solver=$(PREFIX)/ceres-solver-$(CERES_VERSION) --with-protoc=$(PREFIX)/protobuf-$(PROTOBUF_VERSION)/bin/protoc --with-sundials=$(PREFIX)/sundials-$(SUNDIALS_VERSION) --with-wxWidgets=$(PREFIX)/wxWidgets-$(WXWIDGETS_VERSION) --enable-silent-rules)
	$(MAKE) -C var/sequential-version all
	-rm -rf $(PREFIX)/sequential-version
	$(MAKE) -C var/sequential-version install
	touch $@

.DEFAULT_GOAL := all
