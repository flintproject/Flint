# -*- Mode: makefile-gmake; tab-width: 4; indent-tabs-mode: t -*-
include config.mk
include ../deposit.mk
include ../option.mk
include ../version.mk

SQLITE_YEAR = 2020
SQLITE_VSTR = 3320100

$(eval $(call external_library_source,https://sqlite.org/$(SQLITE_YEAR),sqlite-amalgamation-$(SQLITE_VSTR).zip,8d46ef69b96628bedb781bd8309210f2a1f4a353792097302f6b754044e6540f))
$(eval $(call external_library_source,https://sqlite.org/$(SQLITE_YEAR),sqlite-dll-win64-x64-$(SQLITE_VSTR).zip,02dfbb3f9a4b98a221c48263270fd3eebedbc3e8057ae364458cc41d2237e2fc))

$(eval $(call external_library_source,https://ftp.gnu.org/gnu/libmicrohttpd,libmicrohttpd-$(LIBMICROHTTPD_VERSION)-w32-bin.zip,80fe1395a239df71c65df149ee8804f155dc5c2ebd736f28e56bcd289ffafadd))

$(eval $(call external_library_source,https://github.com/wxWidgets/wxWidgets/releases/download/v$(WXWIDGETS_VERSION),wxWidgets-$(WXWIDGETS_VERSION)-headers.7z,a960b589ec18c143139421c2ae091980e5d5e48209faed94882496bf1a8bb6bf))
$(eval $(call external_library_source,https://github.com/wxWidgets/wxWidgets/releases/download/v$(WXWIDGETS_VERSION),wxMSW-$(WXWIDGETS_VERSION)_vc14x_Dev.7z,2349d7a40f97210c943a787e91bec4a38672503342e0daf4212ec79dd94657c3))
$(eval $(call external_library_source,https://github.com/wxWidgets/wxWidgets/releases/download/v$(WXWIDGETS_VERSION),wxMSW-$(WXWIDGETS_VERSION)_vc14x_x64_Dev.7z,cc812f3bafba1a8920cd6617e40c7f177ea6c17e23745986d7589d366a3b77c4))
$(eval $(call external_library_source,https://github.com/wxWidgets/wxWidgets/releases/download/v$(WXWIDGETS_VERSION),wxMSW-$(WXWIDGETS_VERSION)_vc14x_ReleaseDLL.7z,6c88ba2bf44ad835e01d4e31ef751a304bf590e3088569d08abf48cdfc6691a4))
$(eval $(call external_library_source,https://github.com/wxWidgets/wxWidgets/releases/download/v$(WXWIDGETS_VERSION),wxMSW-$(WXWIDGETS_VERSION)_vc14x_x64_ReleaseDLL.7z,bf7a06b6b5e59209d862795f98a834dcec74f051be46f9187206b0250da67375))

UNNECESSARY_LIBRARIES = libmicrohttpd-% wxWidgets-%
EXTERNAL_LIBRARIES = $(filter-out $(UNNECESSARY_LIBRARIES),$(ALL_EXTERNAL_LIBRARIES)) libmicrohttpd-$(LIBMICROHTTPD_VERSION)-w32-bin.zip wxWidgets-$(WXWIDGETS_VERSION)-headers.7z wxMSW-$(WXWIDGETS_VERSION)_$(MSVC_VARIANT)_Dev.7z wxMSW-$(WXWIDGETS_VERSION)_$(MSVC_VARIANT)_ReleaseDLL.7z sqlite-amalgamation-$(SQLITE_VSTR).zip sqlite-dll-win64-x64-$(SQLITE_VSTR).zip
EXTERNAL_LIBRARY_DIRS = $(filter-out $(UNNECESSARY_LIBRARIES),$(ALL_EXTERNAL_LIBRARY_DIRS)) libmicrohttpd-$(LIBMICROHTTPD_VERSION)-w32-bin include lib

.PHONY: all mostlyclean clean distclean download

all: download all.proj $(foreach lib,$(EXTERNAL_LIBRARY_DIRS),tmp/$(lib)) tmp/release-dll tmp/sqlite.header tmp/sqlite.precompiled flint/flint.ico flint/flint.wxs flint/license.rtf flint/third-party-licenses.txt

mostlyclean:
	-rm -rf all.proj tmp/build var/build flint

clean: mostlyclean
	-rm -rf opt release-dll tmp var

distclean: clean
	-rm -f src/*.7z src/*.zip src/*.tar.bz2 src/*.tar.gz

download: $(foreach lib,$(EXTERNAL_LIBRARIES),src/$(lib))

build flint release-dll var:
	install -d $@

tmp: | release-dll var
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

tmp/czmq-$(CZMQ_VERSION): src/czmq-$(CZMQ_VERSION).tar.gz czmq-$(CZMQ_VERSION).patch | tmp
	-rm -rf var/czmq-$(CZMQ_VERSION)
	tar -C var -zxf $<
	patch -d var/czmq-$(CZMQ_VERSION) -p0 < czmq-$(CZMQ_VERSION).patch
	touch $@

tmp/libsbml-$(LIBSBML_VERSION): src/libSBML-$(LIBSBML_VERSION)-core-src.zip | tmp
	-rm -rf var/libsbml-$(LIBSBML_VERSION)
	(cd var && unzip -qq ../$<)
	touch $@

tmp/libxml2-$(LIBXML2_VERSION): src/libxml2-$(LIBXML2_VERSION).tar.gz | tmp
	-rm -rf var/libxml2-$(LIBXML2_VERSION)
	tar -C var -zxf $<
	touch $@

tmp/protobuf-$(PROTOBUF_VERSION): src/protobuf-cpp-$(PROTOBUF_VERSION).tar.gz | tmp
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
	touch $@

tmp/libmicrohttpd-$(LIBMICROHTTPD_VERSION)-w32-bin: src/libmicrohttpd-$(LIBMICROHTTPD_VERSION)-w32-bin.zip | tmp
	-rm -rf var/libmicrohttpd-$(LIBMICROHTTPD_VERSION)-w32-bin
	(cd var && unzip -qq ../$<)
	touch $@

tmp/lib: src/wxMSW-$(WXWIDGETS_VERSION)_$(MSVC_VARIANT)_Dev.7z | tmp
	-rm -rf var/lib
	(cd var && 7z x ../$<)
	touch $@

tmp/include: src/wxWidgets-$(WXWIDGETS_VERSION)-headers.7z | tmp
	-rm -rf var/include
	(cd var && 7z x ../$<)
	touch $@

tmp/release-dll: src/wxMSW-$(WXWIDGETS_VERSION)_$(MSVC_VARIANT)_ReleaseDLL.7z | tmp
	(cd release-dll && 7z x ../$<)
	touch $@

tmp/sqlite.header: src/sqlite-amalgamation-$(SQLITE_VSTR).zip | tmp
	-rm -rf var/sqlite-amalgamation-$(SQLITE_VSTR)
	unzip -d var -qq $<
	touch $@

tmp/sqlite.precompiled: src/sqlite-dll-win64-x64-$(SQLITE_VSTR).zip | tmp
	-rm -f var/sqlite3.*
	unzip -d var -qq $<
	touch $@

flint/flint.ico: flint.ico | flint
	install -p -m 0644 $< $@

flint/flint.wxs: flint.wxs.in | flint
	m4 -DVERSION=$(VERSION) $< > $@

flint/license.rtf: ../doc/license.rtf | flint
	install -p -m 0644 $< $@

flint/third-party-licenses.txt: tmp/ceres-solver-$(CERES_VERSION) tmp/czmq-$(CZMQ_VERSION) tmp/libsbml-$(LIBSBML_VERSION) tmp/libxml2-$(LIBXML2_VERSION) tmp/protobuf-$(PROTOBUF_VERSION) tmp/SBML_odeSolver-$(SOSLIB_VERSION) tmp/sundials-$(SUNDIALS_VERSION) tmp/zeromq-$(ZEROMQ_VERSION) | flint
	echo ';;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;' > $@
	echo ';; Licenses of third-party libraries' >> $@
	echo ';;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;' >> $@
	echo >> $@
	echo ';; Ceres Solver $(CERES_VERSION)' >> $@
	echo >> $@
	cat var/ceres-solver-$(CERES_VERSION)/LICENSE >> $@
	echo >> $@
	echo ';; CZMQ $(CZMQ_VERSION)' >> $@
	echo >> $@
	cat var/czmq-$(CZMQ_VERSION)/LICENSE >> $@
	echo >> $@
	echo ';; GNU libmicrohttpd $(LIBMICROHTTPD_VERSION)' >> $@
	echo >> $@
	echo 'GNU LGPL v2.1 <http://www.gnu.org/licenses/old-licenses/lgpl-2.1.html>' >> $@
	echo >> $@
	echo ';; libSBML $(LIBSBML_VERSION)' >> $@
	echo >> $@
	cat var/libsbml-$(LIBSBML_VERSION)/LICENSE.txt >> $@
	echo >> $@
	echo ';; libxml2 $(LIBXML2_VERSION)' >> $@
	echo >> $@
	cat var/libxml2-$(LIBXML2_VERSION)/Copyright >> $@
	echo >> $@
	echo ';; Protocol Buffers $(PROTOBUF_VERSION)' >> $@
	echo >> $@
	cat var/protobuf-$(PROTOBUF_VERSION)/LICENSE >> $@
	echo >> $@
	echo ';; The SBML ODE Solver Library (SOSlib) $(SOSLIB_VERSION)' >> $@
	echo >> $@
	cat var/SBML_odeSolver-$(SOSLIB_VERSION)/COPYING >> $@
	echo >> $@
	echo ';; SUNDIALS $(SUNDIALS_VERSION)' >> $@
	echo >> $@
	cat var/sundials-$(SUNDIALS_VERSION)/LICENSE >> $@
	echo >> $@
	echo ';; wxWidgets $(WXWIDGETS_VERSION)' >> $@
	echo >> $@
	echo 'wxWindows Library Licence <https://www.wxwidgets.org/about/licence/>' >> $@
	echo >> $@
	echo ';; ZeroMQ $(ZEROMQ_VERSION)' >> $@
	echo >> $@
	cat var/zeromq-$(ZEROMQ_VERSION)/COPYING.LESSER >> $@

all.proj: all.proj.in
	m4 \
		-D__TARGET_ARCH="$(TARGET_ARCH)" \
		-D__BOOST_UNDERSCORE="$(BOOST_UNDERSCORE)" \
		-D__B2_WITHOUT_OPTIONS="$(B2_WITHOUT_OPTIONS)" \
		-D__CERES_VERSION="$(CERES_VERSION)" \
		-D__CLIBSEDML_VERSION="$(CLIBSEDML_VERSION)" \
		-D__CZMQ_VERSION="$(CZMQ_VERSION)" \
		-D__LIBMICROHTTPD_VERSION="$(LIBMICROHTTPD_VERSION)" \
		-D__LIBSBML_VERSION="$(LIBSBML_VERSION)" \
		-D__LIBXML2_VERSION="$(LIBXML2_VERSION)" \
		-D__PROTOBUF_VERSION="$(PROTOBUF_VERSION)" \
		-D__SOSLIB_VERSION="$(SOSLIB_VERSION)" \
		-D__SQLITE_VSTR="$(SQLITE_VSTR)" \
		-D__SUNDIALS_VERSION="$(SUNDIALS_VERSION)" \
		-D__ZEROMQ_VERSION="$(ZEROMQ_VERSION)" \
		$< > $@

.DEFAULT_GOAL := all
