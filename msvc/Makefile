# -*- Mode: makefile-gmake; tab-width: 4; indent-tabs-mode: t -*-
include config.mk
include ../deposit.mk
include ../option.mk
include ../version.mk

$(eval $(call external_library_source,https://github.com/wxWidgets/wxWidgets/releases/download/v$(WXWIDGETS_VERSION),wxWidgets-$(WXWIDGETS_VERSION)-headers.7z,64d598787dba35756c9c66ff0ccbb42d))
$(eval $(call external_library_source,https://github.com/wxWidgets/wxWidgets/releases/download/v$(WXWIDGETS_VERSION),wxMSW-$(WXWIDGETS_VERSION)_vc140_Dev.7z,e26982d01a9f0392cd8c5bae720dc642))
$(eval $(call external_library_source,https://github.com/wxWidgets/wxWidgets/releases/download/v$(WXWIDGETS_VERSION),wxMSW-$(WXWIDGETS_VERSION)_vc140_x64_Dev.7z,7029dadcdafe10070b97fd7848d758a0))

UNNECESSARY_LIBRARIES = wxWidgets-%
EXTERNAL_LIBRARIES = $(filter-out $(UNNECESSARY_LIBRARIES),$(ALL_EXTERNAL_LIBRARIES)) wxWidgets-$(WXWIDGETS_VERSION)-headers.7z wxMSW-$(WXWIDGETS_VERSION)_$(MSVC_VARIANT)_Dev.7z
EXTERNAL_LIBRARY_DIRS = $(filter-out $(UNNECESSARY_LIBRARIES),$(ALL_EXTERNAL_LIBRARY_DIRS)) include lib

.PHONY: all mostlyclean clean distclean download

all: download all.proj $(foreach lib,$(filter-out $(UNNECESSARY_LIBRARIES),$(ALL_EXTERNAL_LIBRARY_DIRS)),tmp/$(lib)) tmp/include tmp/lib

mostlyclean:
	-rm -rf all.proj var/build

clean: mostlyclean
	-rm -rf opt usr tmp/*.installed $(foreach lib,$(EXTERNAL_LIBRARY_DIRS),tmp/$(lib) var/$(lib))

distclean: clean
	-rm -f src/*.7z src/*.zip src/*.tar.bz2 src/*.tar.gz

download: $(foreach lib,$(EXTERNAL_LIBRARIES),src/$(lib))

build var:
	install -d $@

tmp: | var
	install -d $@

tmp/$(BOOST_UNDERSCORE): src/$(BOOST_UNDERSCORE).tar.bz2 | tmp
	-rm -rf var/$(BOOST_UNDERSCORE)
	tar -C var -jxf $<
	touch $@

tmp/ceres-solver-$(CERES_VERSION): src/ceres-solver-$(CERES_VERSION).tar.gz ceres-solver-$(CERES_VERSION).patch | tmp
	-rm -rf var/ceres-solver-$(CERES_VERSION)
	tar -C var -zxf $<
	patch var/ceres-solver-$(CERES_VERSION)/examples/CMakeLists.txt ceres-solver-$(CERES_VERSION).patch
	touch $@

tmp/clibsedml-$(CLIBSEDML_VERSION): src/clibsedml-$(CLIBSEDML_VERSION).tar.gz clibsedml-$(CLIBSEDML_VERSION).patch | tmp
	-rm -rf var/clibsedml-$(CLIBSEDML_VERSION)
	tar -C var -zxf $<
	patch -d var/clibsedml-$(CLIBSEDML_VERSION) -p1 < clibsedml-$(CLIBSEDML_VERSION).patch
	touch $@

tmp/czmq-$(CZMQ_VERSION): src/czmq-$(CZMQ_VERSION).tar.gz | tmp
	-rm -rf var/czmq-$(CZMQ_VERSION)
	tar -C var -zxf $<
	patch var/czmq-$(CZMQ_VERSION)/Findlibzmq.cmake czmq-$(CZMQ_VERSION).patch
	touch $@

tmp/libsbml-$(LIBSBML_VERSION): src/libSBML-$(LIBSBML_VERSION)-core-src.zip | tmp
	-rm -rf var/libsbml-$(LIBSBML_VERSION)
	(cd var && unzip -qq ../$<)
	touch $@

tmp/libxml2-$(LIBXML2_VERSION): src/libxml2-$(LIBXML2_VERSION).tar.gz | tmp
	-rm -rf var/libxml2-$(LIBXML2_VERSION)
	tar -C var -zxf $<
	touch $@

tmp/protobuf-$(PROTOBUF_VERSION): src/protobuf-java-$(PROTOBUF_VERSION).tar.gz | tmp
	-rm -rf var/protobuf-$(PROTOBUF_VERSION)
	tar -C var -zxf $<
	touch $@

tmp/SBML_odeSolver-$(SOSLIB_VERSION): src/SBML_odeSolver-$(SOSLIB_VERSION).tar.gz SBML_odeSolver-$(SOSLIB_VERSION).patch | tmp
	-rm -rf var/SBML_odeSolver-$(SOSLIB_VERSION)
	tar -C var -zxf $<
	patch -d var/SBML_odeSolver-$(SOSLIB_VERSION) -p1 < SBML_odeSolver-$(SOSLIB_VERSION).patch
	touch $@

tmp/sundials-$(SUNDIALS_VERSION): src/sundials-$(SUNDIALS_VERSION).tar.gz sundials-$(SUNDIALS_VERSION).patch | tmp
	-rm -rf var/sundials-$(SUNDIALS_VERSION)
	tar -C var -zxf $<
	patch -d var/sundials-$(SUNDIALS_VERSION) -p1 < sundials-$(SUNDIALS_VERSION).patch
	touch $@

tmp/zeromq-$(ZEROMQ_VERSION): src/zeromq-$(ZEROMQ_VERSION).tar.gz | tmp
	-rm -rf var/zeromq-$(ZEROMQ_VERSION)
	tar -C var -zxf $<
	patch var/zeromq-$(ZEROMQ_VERSION)/ZeroMQConfig.cmake.in zeromq-$(ZEROMQ_VERSION).patch
	touch $@

tmp/lib: src/wxMSW-$(WXWIDGETS_VERSION)_$(MSVC_VARIANT)_Dev.7z
	-rm -rf var/lib
	(cd var && 7z x ../$<)
	touch $@

tmp/include: src/wxWidgets-$(WXWIDGETS_VERSION)-headers.7z
	-rm -rf var/include
	(cd var && 7z x ../$<)
	touch $@

all.proj: all.proj.in
	m4 \
		-D__BOOST_UNDERSCORE="$(BOOST_UNDERSCORE)" \
		-D__B2_WITHOUT_OPTIONS="$(B2_WITHOUT_OPTIONS)" \
		-D__CERES_VERSION="$(CERES_VERSION)" \
		-D__CLIBSEDML_VERSION="$(CLIBSEDML_VERSION)" \
		-D__CZMQ_VERSION="$(CZMQ_VERSION)" \
		-D__LIBSBML_VERSION="$(LIBSBML_VERSION)" \
		-D__LIBXML2_VERSION="$(LIBXML2_VERSION)" \
		-D__PROTOBUF_VERSION="$(PROTOBUF_VERSION)" \
		-D__SOSLIB_VERSION="$(SOSLIB_VERSION)" \
		-D__SUNDIALS_VERSION="$(SUNDIALS_VERSION)" \
		-D__ZEROMQ_VERSION="$(ZEROMQ_VERSION)" \
		$< > $@

.DEFAULT_GOAL := all
