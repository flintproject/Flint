# -*- Mode: makefile-gmake; tab-width: 4; indent-tabs-mode: t -*-
include config.mk
include ../deposit.mk
include ../option.mk
include ../version.mk

EXTERNAL_LIBRARIES = $(ALL_EXTERNAL_LIBRARIES)
EXTERNAL_LIBRARY_DIRS = $(ALL_EXTERNAL_LIBRARY_DIRS)

PREFIX = $(abspath usr)

PKG_CONFIG_PATH = $(PREFIX)/lib/pkgconfig

BOOST_DYLIBS := \
    filesystem \
    program_options \
    system
DYLIBS := \
    libODES.0 \
    libceres.$(CERES_CURRENT) \
    libczmq.$(CZMQ_CURRENT) \
    libmicrohttpd.$(LIBMICROHTTPD_CURRENT) \
    libsbml.$(LIBSBML_VERSION) \
    libsedml.0 \
    libxml2.2 \
    libprotobuf.$(PROTOBUF_CURRENT) \
    libwx_osx_cocoau-3.1 \
    libzmq.$(ZEROMQ_CURRENT)
SUNDIALS_DYLIBS := \
    arkode.1 \
    cvode.2 \
    cvodes.2 \
    ida.2 \
    kinsol.2 \
    nvecserial.2
FLINT_DYLIBS := \
    libflint.0 \
    libflintxx.0 \
    libsqlite3.0
PROGRAMS := \
    csv2isd \
    flint-exec \
    flint-lodbg \
    flint-run \
    flint-tr \
    flint \
    isd2csv \
    isdcut \
    isddiff \
    isdhead \
    isdls \
    isdmerge \
    isdplot \
    isdsort \
    isdstrip \
    isdwc

DMGS = Flint-$(VERSION).dmg

APP = root/Applications/Flint.app

flint_frameworks_boost_dylib_name = $(APP)/Contents/Frameworks/libboost_$(1)-mt.dylib
flint_frameworks_dylib_name = $(APP)/Contents/Frameworks/$(1).dylib
flint_frameworks_sundials_dylib_name = $(APP)/Contents/Frameworks/libsundials_$(1).dylib
flint_macos_program_name = $(APP)/Contents/MacOS/$(1)

define flint_frameworks_boost_dylib
$(call flint_frameworks_boost_dylib_name,$(1)): tmp/$(BOOST_UNDERSCORE).installed tmp/bundled
	install -p -m 0644 $(PREFIX)/lib/libboost_$(1)-mt.dylib $$@
	install_name_tool -id @executable_path/../Frameworks/libboost_$(1)-mt.dylib $$@
	install_name_tool -change @rpath/libboost_system-mt.dylib @executable_path/../Frameworks/libboost_system-mt.dylib $$@
endef

define flint_frameworks_dylib
$(call flint_frameworks_dylib_name,$(1)): $(PREFIX)/lib/$(1).dylib tmp/bundled
	install -p -m 0644 $$< $$@
	install_name_tool -id @executable_path/../Frameworks/$(1).dylib $$@
	install_name_tool -change $(PREFIX)/lib/libxml2.2.dylib @executable_path/../Frameworks/libxml2.2.dylib $$@
	install_name_tool -change $(PREFIX)/lib/libsbml.$(LIBSBML_VERSION).dylib @executable_path/../Frameworks/libsbml.$(LIBSBML_VERSION).dylib $$@
	for name in $(SUNDIALS_DYLIBS); do install_name_tool -change $(PREFIX)/lib/libsundials_$$$$name.dylib @executable_path/../Frameworks/libsundials_$$$$name.dylib $$@; done
	for name in $(SUNDIALS_DYLIBS); do install_name_tool -change libsundials_$$$$name.dylib @executable_path/../Frameworks/libsundials_$$$$name.dylib $$@; done # for SOSlib
	install_name_tool -change $(PREFIX)/lib/libzmq.$(ZEROMQ_CURRENT).dylib @executable_path/../Frameworks/libzmq.$(ZEROMQ_CURRENT).dylib $$@
endef

define flint_frameworks_sundials_dylib
$(call flint_frameworks_sundials_dylib_name,$(1)): tmp/sundials-$(SUNDIALS_VERSION).installed tmp/bundled
	install -p -m 0644 $(PREFIX)/lib/libsundials_$(1).dylib $$@
	install_name_tool -id @executable_path/../Frameworks/libsundials_$(1).dylib $$@
endef

define flint_frameworks_flint_dylib
$(call flint_frameworks_dylib_name,$(1)): opt/lib/$(1).dylib tmp/bundled
	install -p -m 0644 $$< $$@
	install_name_tool -id @executable_path/../Frameworks/$(1).dylib $$@
	for name in $(BOOST_DYLIBS); do install_name_tool -change @rpath/libboost_$$$$name-mt.dylib @executable_path/../Frameworks/libboost_$$$$name-mt.dylib $$@; done
	for name in $(DYLIBS); do install_name_tool -change $(PREFIX)/lib/$$$$name.dylib @executable_path/../Frameworks/$$$$name.dylib $$@; done
	for name in $(SUNDIALS_DYLIBS); do install_name_tool -change $(PREFIX)/lib/libsundials_$$$$name.dylib @executable_path/../Frameworks/libsundials_$$$$name.dylib $$@; done
	for name in $(FLINT_DYLIBS); do install_name_tool -change $(CURDIR)/opt/lib/$$$$name.dylib @executable_path/../Frameworks/$$$$name.dylib $$@; done
endef

define flint_macos_program
$(call flint_macos_program_name,$(1)): opt/bin/$(1) tmp/bundled
	install -p $$< $$@
	for name in $(BOOST_DYLIBS); do install_name_tool -change @rpath/libboost_$$$$name-mt.dylib @executable_path/../Frameworks/libboost_$$$$name-mt.dylib $$@; done
	for name in $(DYLIBS); do install_name_tool -change $(PREFIX)/lib/$$$$name.dylib @executable_path/../Frameworks/$$$$name.dylib $$@; done
	for name in $(SUNDIALS_DYLIBS); do install_name_tool -change $(PREFIX)/lib/libsundials_$$$$name.dylib @executable_path/../Frameworks/libsundials_$$$$name.dylib $$@; done
	for name in $(FLINT_DYLIBS); do install_name_tool -change $(CURDIR)/opt/lib/$$$$name.dylib @executable_path/../Frameworks/$$$$name.dylib $$@; done
endef

.PHONY: all check mostlyclean clean distclean download external

all: download external $(DMGS)

check:
	env DYLD_LIBRARY_PATH=$(PREFIX)/lib $(MAKE) -C build check

mostlyclean:
	-rm -rf tmp/bundled Flint-*.dmg Flint-*/ flint-component.pkg resources root opt build build.xml ../configure

clean: mostlyclean
	-rm -rf usr $(foreach lib,$(EXTERNAL_LIBRARY_DIRS),tmp/$(lib).installed tmp/$(lib) var/$(lib))

distclean: clean
	-rm -f $(foreach lib,$(EXTERNAL_LIBRARIES),src/$(lib))

download: $(foreach lib,$(EXTERNAL_LIBRARIES),src/$(lib))

external: $(foreach lib,$(BOOST_UNDERSCORE) ceres-solver-$(CERES_VERSION) clibsedml-$(CLIBSEDML_VERSION) czmq-$(CZMQ_VERSION) libmicrohttpd-$(LIBMICROHTTPD_VERSION) libsbml-$(LIBSBML_VERSION) libxml2-$(LIBXML2_VERSION) protobuf-$(PROTOBUF_VERSION) SBML_odeSolver-$(SOSLIB_VERSION) sundials-$(SUNDIALS_VERSION) wxWidgets-$(WXWIDGETS_VERSION) zeromq-$(ZEROMQ_VERSION),tmp/$(lib).installed)

var:
	install -d $@

tmp: | var
	install -d $@

tmp/$(BOOST_UNDERSCORE): src/$(BOOST_UNDERSCORE).tar.bz2 | tmp
	-rm -rf var/$(BOOST_UNDERSCORE)
	tar -C var -jxf $<
	touch $@

tmp/ceres-solver-$(CERES_VERSION): src/ceres-solver-$(CERES_VERSION).tar.gz | tmp
	-rm -rf var/ceres-solver-$(CERES_VERSION)
	tar -C var -zxf $<
	touch $@

tmp/clibsedml-$(CLIBSEDML_VERSION): src/clibsedml-$(CLIBSEDML_VERSION).tar.gz | tmp
	-rm -rf var/clibsedml-$(CLIBSEDML_VERSION)
	tar -C var -zxf $<
	touch $@

tmp/czmq-$(CZMQ_VERSION): src/czmq-$(CZMQ_VERSION).tar.gz | tmp
	-rm -rf var/czmq-$(CZMQ_VERSION)
	tar -C var -zxf $<
	rm var/czmq-$(CZMQ_VERSION)/src/platform.h
	touch $@

tmp/libmicrohttpd-$(LIBMICROHTTPD_VERSION): src/libmicrohttpd-$(LIBMICROHTTPD_VERSION).tar.gz | tmp
	-rm -rf var/libmicrohttpd-$(LIBMICROHTTPD_VERSION)
	tar -C var -zxf $<
	touch $@

tmp/libsbml-$(LIBSBML_VERSION): src/libSBML-$(LIBSBML_VERSION)-core-src.zip libsbml-$(LIBSBML_VERSION).patch | tmp
	-rm -rf var/libsbml-$(LIBSBML_VERSION)
	(cd var && unzip -qq ../$<)
	patch var/libsbml-$(LIBSBML_VERSION)/Makefile.in libsbml-$(LIBSBML_VERSION).patch
	touch $@

tmp/libxml2-$(LIBXML2_VERSION): src/libxml2-$(LIBXML2_VERSION).tar.gz | tmp
	-rm -rf var/libxml2-$(LIBXML2_VERSION)
	tar -C var -zxf $<
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
	-rm -rf var/sundials-$(SUNDIALS_VERSION)
	tar -C var -zxf $<
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
	(cd var/$(BOOST_UNDERSCORE) && export MACOSX_DEPLOYMENT_TARGET=10.9 && ./bootstrap.sh --with-toolset=clang --without-icu && ./b2 -d0 --prefix=$(PREFIX) $(B2_WITHOUT_OPTIONS) --layout=tagged toolset=clang cflags='-arch x86_64 -mmacosx-version-min=10.9' cxxflags='-std=c++11 -stdlib=libc++ -arch x86_64 -mmacosx-version-min=10.9' linkflags='-stdlib=libc++ -arch x86_64 -mmacosx-version-min=10.9' link=shared variant=release threading=multi install)
	touch $@

tmp/ceres-solver-$(CERES_VERSION).installed: tmp/ceres-solver-$(CERES_VERSION)
	install -d var/ceres-solver-$(CERES_VERSION)/build
	(cd var/ceres-solver-$(CERES_VERSION)/build && export MACOSX_DEPLOYMENT_TARGET=10.9 && CC='clang' CXX='clang++' CFLAGS='-arch x86_64 -mmacosx-version-min=10.9' CXXFLAGS='-std=c++11 -stdlib=libc++ -arch x86_64 -mmacosx-version-min=10.9' LDFLAGS='-stdlib=libc++ -arch x86_64 -mmacosx-version-min=10.9' cmake -DCMAKE_INSTALL_PREFIX=$(PREFIX) -DEIGEN_INCLUDE_DIR=$(abspath ../include) $(CERES_CMAKE_OPTIONS) .. && $(MAKE) all install)
	touch $@

tmp/clibsedml-$(CLIBSEDML_VERSION).installed: tmp/clibsedml-$(CLIBSEDML_VERSION) tmp/libxml2-$(LIBXML2_VERSION).installed
	(cd var/clibsedml-$(CLIBSEDML_VERSION) && autoreconf -i -f && export MACOSX_DEPLOYMENT_TARGET=10.9 && ./configure --prefix=$(PREFIX) --with-xml-prefix=$(PREFIX) $(CLIBSEDML_CONFIGURE_OPTIONS) CC='clang' CFLAGS='-Wall -W -arch x86_64 -mmacosx-version-min=10.9' LDFLAGS='-arch x86_64 -mmacosx-version-min=10.9' && $(MAKE) all install)
	touch $@

tmp/czmq-$(CZMQ_VERSION).installed: tmp/czmq-$(CZMQ_VERSION) tmp/zeromq-$(ZEROMQ_VERSION).installed
	(cd var/czmq-$(CZMQ_VERSION) && export MACOSX_DEPLOYMENT_TARGET=10.9 && ./configure --prefix=$(PREFIX) $(CZMQ_CONFIGURE_OPTIONS) CC='clang' CFLAGS='-arch x86_64 -mmacosx-version-min=10.9' LDFLAGS='-arch x86_64 -mmacosx-version-min=10.9 -Wl,-headerpad_max_install_names' PKG_CONFIG=$(PKG_CONFIG) PKG_CONFIG_PATH=$(PKG_CONFIG_PATH) && $(MAKE) all install)
	touch $@

tmp/libmicrohttpd-$(LIBMICROHTTPD_VERSION).installed: tmp/libmicrohttpd-$(LIBMICROHTTPD_VERSION)
	(cd var/libmicrohttpd-$(LIBMICROHTTPD_VERSION) && export MACOSX_DEPLOYMENT_TARGET=10.9 && ./configure --prefix=$(PREFIX) $(LIBMICROHTTPD_CONFIGURE_OPTIONS) CC='clang' CFLAGS='-arch x86_64 -mmacosx-version-min=10.9' LDFLAGS='-arch x86_64 -mmacosx-version-min=10.9 -Wl,-headerpad_max_install_names' && $(MAKE) all install)
	touch $@

tmp/protobuf-$(PROTOBUF_VERSION).installed: tmp/protobuf-$(PROTOBUF_VERSION)
	(cd var/protobuf-$(PROTOBUF_VERSION) && export MACOSX_DEPLOYMENT_TARGET=10.9 && ./configure --prefix=$(PREFIX) $(PROTOBUF_CONFIGURE_OPTONS) CC='clang' CXX='clang++' CFLAGS='-arch x86_64 -mmacosx-version-min=10.9' CXXFLAGS='-std=c++11 -stdlib=libc++ -arch x86_64 -mmacosx-version-min=10.9' LDFLAGS='-stdlib=libc++ -arch x86_64 -mmacosx-version-min=10.9' LIBS='-lc++ -lc++abi' && $(MAKE) all install)
	touch $@

tmp/libxml2-$(LIBXML2_VERSION).installed: tmp/libxml2-$(LIBXML2_VERSION)
	(cd var/libxml2-$(LIBXML2_VERSION) && export MACOSX_DEPLOYMENT_TARGET=10.9 && ./configure --prefix=$(PREFIX) $(LIBXML2_CONFIGURE_OPTIONS) CC='clang' CXX='clang++' CFLAGS='-arch x86_64 -mmacosx-version-min=10.9' CXXFLAGS='-std=c++11 -stdlib=libc++ -arch x86_64 -mmacosx-version-min=10.9' LDFLAGS='-stdlib=libc++ -arch x86_64 -mmacosx-version-min=10.9' && $(MAKE) all install)
	touch $@

tmp/sundials-$(SUNDIALS_VERSION).installed: tmp/sundials-$(SUNDIALS_VERSION)
	install -d var/sundials-$(SUNDIALS_VERSION)/build
	(cd var/sundials-$(SUNDIALS_VERSION)/build && export MACOSX_DEPLOYMENT_TARGET=10.9 && CC='clang' CXX='clang++' CFLAGS='-arch x86_64 -mmacosx-version-min=10.9' CXXFLAGS='-std=c++11 -stdlib=libc++ -arch x86_64 -mmacosx-version-min=10.9' LDFLAGS='-stdlib=libc++ -arch x86_64 -mmacosx-version-min=10.9' cmake -DCMAKE_INSTALL_PREFIX=$(PREFIX) $(SUNDIALS_CMAKE_OPTIONS) .. && $(MAKE) all install)
	touch $@

tmp/wxWidgets-$(WXWIDGETS_VERSION).installed: tmp/wxWidgets-$(WXWIDGETS_VERSION)
	(cd var/wxWidgets-$(WXWIDGETS_VERSION) && export MACOSX_DEPLOYMENT_TARGET=10.9 && ./configure --prefix=$(PREFIX) $(WXWIDGETS_CONFIGURE_OPTIONS) CC='clang' CXX='clang++' CFLAGS='-arch x86_64 -mmacosx-version-min=10.9' CXXFLAGS='-std=c++11 -stdlib=libc++ -arch x86_64 -mmacosx-version-min=10.9' LDFLAGS='-stdlib=libc++ -arch x86_64 -mmacosx-version-min=10.9' && $(MAKE) all install)
	touch $@

tmp/libsbml-$(LIBSBML_VERSION).installed: tmp/libsbml-$(LIBSBML_VERSION) tmp/libxml2-$(LIBXML2_VERSION).installed
	(cd var/libsbml-$(LIBSBML_VERSION) && export MACOSX_DEPLOYMENT_TARGET=10.9 && ./configure --prefix=$(PREFIX) --with-libxml=$(PREFIX) $(LIBSBML_CONFIGURE_OPTIONS) CC='clang' CXX='clang++' CFLAGS='-arch x86_64 -mmacosx-version-min=10.9' CXXFLAGS='-std=c++11 -stdlib=libc++ -arch x86_64 -mmacosx-version-min=10.9' LDFLAGS='-stdlib=libc++ -arch x86_64 -mmacosx-version-min=10.9' LIBS='-lc++' && $(MAKE) all install)
	unlink $(PREFIX)/lib/libsbml.5.dylib # remove the alias to make the dylib name unique
	install_name_tool -id $(PREFIX)/lib/libsbml.$(LIBSBML_VERSION).dylib $(PREFIX)/lib/libsbml.$(LIBSBML_VERSION).dylib
	touch $@

tmp/SBML_odeSolver-$(SOSLIB_VERSION).installed: tmp/SBML_odeSolver-$(SOSLIB_VERSION) tmp/sundials-$(SUNDIALS_VERSION).installed tmp/libsbml-$(LIBSBML_VERSION).installed
	(cd var/SBML_odeSolver-$(SOSLIB_VERSION) && autoreconf -i && export MACOSX_DEPLOYMENT_TARGET=10.9 && ./configure --prefix=$(PREFIX) --with-libsbml=$(PREFIX) --with-libxml2=$(PREFIX) --with-sundials=$(PREFIX) $(SOSLIB_CONFIGURE_OPTIONS) CC='clang' CFLAGS='-arch x86_64 -mmacosx-version-min=10.9 -Wl,-headerpad_max_install_names' LDFLAGS='-arch x86_64 -mmacosx-version-min=10.9' && $(MAKE) all install)
	touch $@

tmp/zeromq-$(ZEROMQ_VERSION).installed: tmp/zeromq-$(ZEROMQ_VERSION)
	(cd var/zeromq-$(ZEROMQ_VERSION) && export MACOSX_DEPLOYMENT_TARGET=10.9 && ./configure --prefix=$(PREFIX) $(ZEROMQ_CONFIGURE_OPTIONS) CC='clang' CXX='clang++' CFLAGS='-Wall -W -arch x86_64 -mmacosx-version-min=10.9' CXXFLAGS='-Wall -W -stdlib=libc++ -arch x86_64 -mmacosx-version-min=10.9' LDFLAGS='-arch x86_64 -mmacosx-version-min=10.9' && $(MAKE) all install)
	touch $@

# for Flint

../configure: ../configure.ac
	(cd .. && autoreconf -i -f)

build.xml: build.xml.in
	m4 -DVERSION=$(VERSION) $< > $@

build/Makefile: ../configure
	install -d $(dir $@)
	(cd $(dir $@) && ../../configure --prefix=$(CURDIR)/opt --with-boost=$(PREFIX) --with-ceres-solver=$(PREFIX) --with-protoc=$(PREFIX)/bin/protoc --with-xml-prefix=$(PREFIX) --with-protoc=$(PREFIX)/bin/protoc --with-sundials=$(PREFIX) --with-wxWidgets=$(PREFIX) --enable-release --enable-silent-rules CC='clang' CXX='clang++' CFLAGS='-arch x86_64 -mmacosx-version-min=10.9' CXXFLAGS='-stdlib=libc++ -arch x86_64 -mmacosx-version-min=10.9' LDFLAGS='-stdlib=libc++ -arch x86_64 -mmacosx-version-min=10.9 -headerpad_max_install_names' PKG_CONFIG=$(PKG_CONFIG) PKG_CONFIG_PATH=$(PKG_CONFIG_PATH))

$(foreach p,$(PROGRAMS),opt/bin/$(p)) $(foreach d,$(FLINT_DYLIBS),opt/lib/$(d).dylib): build/Makefile
	$(MAKE) -C build all install-exec

$(foreach d,$(BOOST_DYLIBS),$(eval $(call flint_frameworks_boost_dylib,$(d))))

$(foreach d,$(DYLIBS),$(eval $(call flint_frameworks_dylib,$(d))))

$(foreach d,$(SUNDIALS_DYLIBS),$(eval $(call flint_frameworks_sundials_dylib,$(d))))

$(foreach d,$(FLINT_DYLIBS),$(eval $(call flint_frameworks_flint_dylib,$(d))))

$(APP)/Contents $(APP)/Contents/Frameworks $(APP)/Contents/MacOS:
	install -d $@

tmp/bundled: $(APP)/Contents/Info.plist $(APP)/Contents/PkgInfo $(APP)/Contents/Resources/flint.icns | $(APP)/Contents/Frameworks $(APP)/Contents/MacOS
	touch $@

$(APP)/Contents/Info.plist: Info.plist | $(APP)/Contents
	install -p -m 0644 $< $@

$(APP)/Contents/PkgInfo: | $(APP)/Contents
	touch $@ # FIXME

$(APP)/Contents/Resources/flint.icns: ../flint.icns | $(APP)/Contents
	install -d $(dir $@)
	install -p -m 0644 $< $@

$(foreach p,$(PROGRAMS),$(eval $(call flint_macos_program,$(p))))

$(APP)/Contents/Resources/third-party-licenses.txt: tmp/ceres-solver-$(CERES_VERSION) tmp/libsbml-$(LIBSBML_VERSION) tmp/libxml2-$(LIBXML2_VERSION) tmp/protobuf-$(PROTOBUF_VERSION) tmp/SBML_odeSolver-$(SOSLIB_VERSION) tmp/sundials-$(SUNDIALS_VERSION)
	install -d $(dir $@)
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
	cat var/libmicrohttpd-$(LIBMICROHTTPD_VERSION)/COPYING >> $@
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
	cat var/wxWidgets-$(WXWIDGETS_VERSION)/docs/licence.txt >> $@
	echo >> $@
	echo ';; ZeroMQ $(ZEROMQ_VERSION)' >> $@
	echo >> $@
	cat var/zeromq-$(ZEROMQ_VERSION)/COPYING.LESSER >> $@

flint-component.pkg: component.plist $(foreach d,$(BOOST_DYLIBS),$(call flint_frameworks_boost_dylib_name,$(d))) $(foreach d,$(DYLIBS) $(FLINT_DYLIBS),$(call flint_frameworks_dylib_name,$(d))) $(foreach d,$(SUNDIALS_DYLIBS),$(call flint_frameworks_sundials_dylib_name,$(d))) $(foreach p,$(PROGRAMS),$(call flint_macos_program_name,$(p))) $(APP)/Contents/Resources/third-party-licenses.txt $(APP)/Contents/Resources/flint.icns $(APP)/Contents/Info.plist $(APP)/Contents/PkgInfo
	pkgbuild --quiet --identifier jp.oist.flint --component-plist $< --root root $@

Flint-$(VERSION) resources:
	install -d $@

resources/license.rtf: ../doc/license.rtf | resources
	install -p -m 0644 $< $@

Flint-$(VERSION)/Flint-$(VERSION).pkg: distribution.xml flint-component.pkg resources/license.rtf | Flint-$(VERSION)
	productbuild --quiet --distribution $< --resources resources $@

Flint-$(VERSION).dmg: Flint-$(VERSION)/Flint-$(VERSION).pkg
	hdiutil create -fs HFS+ -ov -srcfolder Flint-$(VERSION) $@
	install -p -m 0644 $@ flint-`date +%Y%m%d`.dmg

.DEFAULT_GOAL := all
