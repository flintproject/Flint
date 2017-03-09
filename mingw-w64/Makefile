# -*- Mode: makefile-gmake; tab-width: 4; indent-tabs-mode: t -*-
include config.mk
include ../deposit.mk
include ../option.mk
include ../version.mk
include header.mk

EXTERNAL_LIBRARIES = $(ALL_EXTERNAL_LIBRARIES)
EXTERNAL_LIBRARY_DIRS = $(ALL_EXTERNAL_LIBRARY_DIRS)

PREFIX = $(abspath opt)

PKG_CONFIG_PATH = $(PREFIX)/lib/pkgconfig

flint_jar_name = flint/lib/$(1).jar
flint_dll_name = flint/$(1).dll
flint_exe_name = flint/$(1).exe

define flint_jar
$(call flint_jar_name,$(1)): ../flint/dist/lib/$(1).jar | flint/lib
	install -p -m 0644 $$< $$@
endef

define flint_bin_dll
$(call flint_dll_name,$(1)): opt/bin/$(1).dll | flint
	install -p $$< $$@
endef

define flint_lib_dll
$(call flint_dll_name,$(1)): opt/lib/$(1).dll | flint
	install -p $$< $$@
endef

define flint_exe
$(call flint_exe_name,$(1)): opt/bin/$(1).exe | flint
	install -p $$< $$@
	strip $$@
endef

.PHONY: all mostlyclean clean distclean download

all: download flint.zip

mostlyclean:
	-rm -rf flint.zip flint var/flint ../configure
	ant -f ../flint/build.xml clean

clean: mostlyclean
	-rm -rf opt usr tmp/*.installed $(foreach lib,$(EXTERNAL_LIBRARY_DIRS),tmp/$(lib) var/$(lib)) build/protobuf-$(PROTOBUF_VERSION)

distclean: clean
	-rm -f src/*.zip src/*.tar.bz2 src/*.tar.gz

download: $(foreach lib,$(EXTERNAL_LIBRARIES),src/$(lib))

build var:
	install -d $@

var/toolchain.cmake: toolchain.cmake.in config.mk | var
	echo 'set(COMPILER_PREFIX "$(HOST)")' > $@
	cat $< >> $@

tmp: | var
	install -d $@

tmp/$(BOOST_UNDERSCORE): src/$(BOOST_UNDERSCORE).tar.bz2 | tmp
	-rm -rf var/$(BOOST_UNDERSCORE)
	tar -C var -jxf $<
	touch $@

tmp/clibsedml-$(CLIBSEDML_VERSION): src/clibsedml-$(CLIBSEDML_VERSION).tar.gz | tmp
	-rm -rf var/clibsedml-$(CLIBSEDML_VERSION)
	tar -C var -zxf $<
	touch $@

tmp/czmq-$(CZMQ_VERSION): src/czmq-$(CZMQ_VERSION).tar.gz | tmp
	-rm -rf var/czmq-$(CZMQ_VERSION)
	tar -C var -zxf $<
	patch var/czmq-$(CZMQ_VERSION)/configure czmq-$(CZMQ_VERSION)-configure.patch
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
	patch var/zeromq-$(ZEROMQ_VERSION)/src/windows.hpp zeromq-$(ZEROMQ_VERSION)-windows.patch
	touch $@

tmp/$(BOOST_UNDERSCORE).installed: tmp/$(BOOST_UNDERSCORE)
	(cd var/$(BOOST_UNDERSCORE) && ./bootstrap.sh --prefix=$(PREFIX) --with-toolset=gcc --without-icu)
	echo "using gcc : : $(HOST)-g++" > var/$(BOOST_UNDERSCORE)/user-config.jam
	echo "        :" >> var/$(BOOST_UNDERSCORE)/user-config.jam
	echo "        <rc>$(HOST)-windres" >> var/$(BOOST_UNDERSCORE)/user-config.jam
	echo ";" >> var/$(BOOST_UNDERSCORE)/user-config.jam
	(cd var/$(BOOST_UNDERSCORE) && ./b2 -d0 --prefix=$(PREFIX) $(B2_WITHOUT_OPTIONS) --layout=tagged --user-config=user-config.jam -j 2 -sNO_BZIP2=1 -q toolset=gcc target-os=windows variant=release threading=multi threadapi=pthread link=shared runtime-link=shared install)
	touch $@

tmp/clibsedml-$(CLIBSEDML_VERSION).installed: tmp/clibsedml-$(CLIBSEDML_VERSION) tmp/libxml2-$(LIBXML2_VERSION).installed
	(cd var/clibsedml-$(CLIBSEDML_VERSION) && autoreconf -i -f && env CFLAGS='-std=c99 -Wall -W' ./configure --host=$(HOST) --prefix=$(PREFIX) --with-xml-prefix=$(PREFIX) --disable-xmltest $(CLIBSEDML_CONFIGURE_OPTIONS))
	$(MAKE) -C var/clibsedml-$(CLIBSEDML_VERSION) all install
	touch $@

tmp/czmq-$(CZMQ_VERSION).installed: tmp/czmq-$(CZMQ_VERSION) tmp/zeromq-$(ZEROMQ_VERSION).installed
	(cd var/czmq-$(CZMQ_VERSION) && ./configure --host=$(HOST) --prefix=$(PREFIX) $(CZMQ_CONFIGURE_OPTIONS) PKG_CONFIG_PATH=$(PKG_CONFIG_PATH) LIBS='-liphlpapi -lrpcrt4 -lws2_32')
	$(MAKE) -C var/czmq-$(CZMQ_VERSION) all install
	touch $@

usr/bin/protoc: src/protobuf-java-$(PROTOBUF_VERSION).tar.gz | build
	-rm -rf build/protobuf-$(PROTOBUF_VERSION)
	tar -C build -zxf $<
	(cd build/protobuf-$(PROTOBUF_VERSION) && ./configure --prefix=$(abspath usr) $(PROTOBUF_CONFIGURE_OPTONS))
	$(MAKE) -C build/protobuf-$(PROTOBUF_VERSION) all install

tmp/protobuf-$(PROTOBUF_VERSION).installed: tmp/protobuf-$(PROTOBUF_VERSION) | usr/bin/protoc
	(cd var/protobuf-$(PROTOBUF_VERSION) && ./configure --host=$(HOST) --prefix=$(PREFIX) --with-protoc=$(abspath usr/bin/protoc) $(PROTOBUF_CONFIGURE_OPTONS))
	$(MAKE) -C var/protobuf-$(PROTOBUF_VERSION) all install
	touch $@

tmp/libxml2-$(LIBXML2_VERSION).installed: tmp/libxml2-$(LIBXML2_VERSION)
	(cd var/libxml2-$(LIBXML2_VERSION) && ./configure --host=$(HOST) --prefix=$(PREFIX) $(LIBXML2_CONFIGURE_OPTIONS))
	$(MAKE) -C var/libxml2-$(LIBXML2_VERSION) all install
	touch $@

tmp/sundials-$(SUNDIALS_VERSION).installed: tmp/sundials-$(SUNDIALS_VERSION) var/toolchain.cmake
	install -d var/sundials-$(SUNDIALS_VERSION)/build
	(cd var/sundials-$(SUNDIALS_VERSION)/build && cmake --host=$(HOST) -DCMAKE_INSTALL_PREFIX=$(PREFIX) -DCMAKE_TOOLCHAIN_FILE=$(CURDIR)/var/toolchain.cmake $(SUNDIALS_CMAKE_OPTIONS) ..)
	$(MAKE) -C var/sundials-$(SUNDIALS_VERSION)/build all install
	touch $@

tmp/wxWidgets-$(WXWIDGETS_VERSION).installed: tmp/wxWidgets-$(WXWIDGETS_VERSION)
	(cd var/wxWidgets-$(WXWIDGETS_VERSION) && ./configure --host=$(HOST) --prefix=$(PREFIX) $(WXWIDGETS_CONFIGURE_OPTIONS))
	$(MAKE) -C var/wxWidgets-$(WXWIDGETS_VERSION) all install
	touch $@

tmp/libsbml-$(LIBSBML_VERSION).installed: var/toolchain.cmake tmp/libsbml-$(LIBSBML_VERSION) tmp/libxml2-$(LIBXML2_VERSION).installed
	mkdir -p var/libsbml-$(LIBSBML_VERSION)/build
	(cd var/libsbml-$(LIBSBML_VERSION)/build && cmake --host=$(HOST) -DCMAKE_INSTALL_PREFIX=$(PREFIX) -DCMAKE_TOOLCHAIN_FILE=$(CURDIR)/var/toolchain.cmake -DLIBXML_INCLUDE_DIR=$(PREFIX)/include/libxml2 -DLIBXML_LIBRARY=$(PREFIX)/bin/libxml2-2.dll ..)
	$(MAKE) -C var/libsbml-$(LIBSBML_VERSION)/build all install
	touch $@

tmp/SBML_odeSolver-$(SOSLIB_VERSION).installed: tmp/SBML_odeSolver-$(SOSLIB_VERSION) tmp/sundials-$(SUNDIALS_VERSION).installed tmp/libsbml-$(LIBSBML_VERSION).installed
	(cd var/SBML_odeSolver-$(SOSLIB_VERSION) && autoreconf -i -f && ./configure --host=$(HOST) --prefix=$(PREFIX) --with-libsbml=$(PREFIX) --with-libxml2=$(PREFIX) --with-sundials=$(PREFIX) $(SOSLIB_CONFIGURE_OPTIONS) CPPFLAGS='-DSBML_ODESOLVER_EXPORTS=1')
	-$(MAKE) -C var/SBML_odeSolver-$(SOSLIB_VERSION) install
	touch $@

tmp/zeromq-$(ZEROMQ_VERSION).installed: tmp/zeromq-$(ZEROMQ_VERSION)
	(cd var/zeromq-$(ZEROMQ_VERSION) && ./configure --host=$(HOST) --prefix=$(PREFIX) $(ZEROMQ_CONFIGURE_OPTIONS))
	$(MAKE) -C var/zeromq-$(ZEROMQ_VERSION) all install
	touch $@

../flint/dist/flint.jar: $(foreach e,$(EXES),opt/bin/$(e).exe)
	ant -f ../flint/build.xml jar

../configure: ../configure.ac
	(cd .. && autoreconf -i -f)

flint opt usr var/flint:
	install -d $@

flint/lib: | flint
	install -d $@

var/flint/Makefile: tmp/$(BOOST_UNDERSCORE).installed tmp/clibsedml-$(CLIBSEDML_VERSION).installed tmp/czmq-$(CZMQ_VERSION).installed tmp/protobuf-$(PROTOBUF_VERSION).installed tmp/SBML_odeSolver-$(SOSLIB_VERSION).installed tmp/wxWidgets-$(WXWIDGETS_VERSION).installed ../configure | usr/bin/protoc var/flint
	(cd var/flint && env CPPFLAGS='-DWIN32_LEAN_AND_MEAN -D_WIN32_WINNT=0x0600 -DWIN32 -DSBML_ODESOLVER_EXPORTS=1' LIBS='-lole32 -loleaut32 -lmswsock -lws2_32' PKG_CONFIG_PATH=$(PKG_CONFIG_PATH) LIBSBML_CFLAGS=$(PREFIX)/include LIBSBML_LIBS='-lsbml' SOSLIB_CFLAGS=$(PREFIX)/include SOSLIB_LIBS='-lODES' ../../../configure --host=$(HOST) --prefix=$(PREFIX) --with-boost=$(PREFIX) --with-xml-prefix=$(PREFIX) --disable-xmltest --with-protoc=$(abspath usr/bin/protoc) --with-sundials=$(PREFIX) --with-wxWidgets=$(PREFIX) --enable-release --enable-silent-rules --enable-shared --disable-static)

$(foreach d,$(FLINT_DLLS),opt/bin/$(d).dll) $(foreach e,$(EXES),opt/bin/$(e).exe): $(foreach d,$(BIN_DLLS) $(BOOST_DLLS),$(call flint_dll_name,$(d))) tmp/sundials-$(SUNDIALS_VERSION).installed tmp/SBML_odeSolver-$(SOSLIB_VERSION).installed var/flint/Makefile
	$(MAKE) -C var/flint all install-exec

flint/flint.jar: ../flint/dist/flint.jar | flint
	install -p -m 0644 $< $@

$(foreach j,$(JARS),$(eval $(call flint_jar,$(j))))

opt/bin/libODES-0.dll: tmp/SBML_odeSolver-$(SOSLIB_VERSION).installed

opt/bin/libczmq.dll: tmp/czmq-$(CZMQ_VERSION).installed

opt/bin/libprotobuf-$(PROTOBUF_CURRENT).dll: tmp/protobuf-$(PROTOBUF_VERSION).installed

opt/bin/libsedml-0.dll: tmp/clibsedml-$(CLIBSEDML_VERSION).installed

opt/bin/libsbml.all: tmp/libsbml-$(LIBSBML_VERSION).installed

opt/bin/libxml2-2.dll: tmp/libxml2-$(LIBXML2_VERSION).installed

opt/bin/libzmq.dll: tmp/zeromq-$(ZEROMQ_VERSION).installed

$(foreach d,$(BIN_DLLS) $(FLINT_DLLS),$(eval $(call flint_bin_dll,$(d))))

$(foreach d,$(BOOST_DLLS),opt/lib/$(d).dll): tmp/$(BOOST_UNDERSCORE).installed

$(foreach d,$(BOOST_DLLS) $(SUNDIALS_DLLS),$(eval $(call flint_lib_dll,$(d))))

$(foreach d,$(SUNDIALS_DLLS),opt/lib/$(d)): tmp/sundials-$(SUNDIALS_VERSION).installed

$(eval $(call flint_lib_dll,wxmsw310u_gcc_custom))

opt/lib/wxmsw310u_gcc_custom.dll: tmp/wxWidgets-$(WXWIDGETS_VERSION).installed

$(foreach e,$(EXES),$(eval $(call flint_exe,$(e))))

flint/$(LIBGCC_DLL): $(LIBGCC_DIR)/$(LIBGCC_DLL) | flint
	install -p -m 0755 $< $@

flint/$(LIBSTDCXX_DLL): $(LIBSTDCXX_DIR)/$(LIBSTDCXX_DLL) | flint
	install -p -m 0755 $< $@

flint/$(LIBWINPTHREAD_DLL): $(LIBWINPTHREAD_DIR)/$(LIBWINPTHREAD_DLL) | flint
	install -p -m 0755 $< $@

flint/Makefile: flint.mk | flint
	install -p -m 0644 $< $@

flint/header.mk: header.mk | flint
	install -p -m 0644 $< $@

flint/flint.ico: flint.ico | flint
	install -p -m 0644 $< $@

flint/flint.xml: flint.xml | flint
	install -p -m 0644 $< $@

flint/flint.wxs: flint.wxs.in | flint
	m4 -DVERSION=$(VERSION) -DLIBGCC_DLL=$(LIBGCC_DLL) -DLIBSTDCXX_DLL=$(LIBSTDCXX_DLL) -DLIBWINPTHREAD_DLL=$(LIBWINPTHREAD_DLL) -DPROTOBUF_CURRENT=$(PROTOBUF_CURRENT) $< > $@

flint/icon.png: ../flint/src/jp/oist/flint/image/icon.png | flint
	install -p -m 0644 $< $@

flint/license.rtf: ../doc/license.rtf | flint
	install -p -m 0644 $< $@

flint/third-party-licenses.txt: tmp/libsbml-$(LIBSBML_VERSION) tmp/libxml2-$(LIBXML2_VERSION) tmp/protobuf-$(PROTOBUF_VERSION) tmp/SBML_odeSolver-$(SOSLIB_VERSION) tmp/sundials-$(SUNDIALS_VERSION) tmp/wxWidgets-$(WXWIDGETS_VERSION) | flint
	echo ';;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;' > $@
	echo ';; Licenses of third-party libraries' >> $@
	echo ';;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;' >> $@
	echo >> $@
	echo ';; CZMQ $(CZMQ_VERSION)' >> $@
	echo >> $@
	cat var/czmq-$(CZMQ_VERSION)/LICENSE >> $@
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

flint.zip: $(foreach e,$(EXES),$(call flint_exe_name,$(e))) $(foreach d,$(BOOST_DLLS) $(SUNDIALS_DLLS) wxmsw310u_gcc_custom $(FLINT_DLLS),$(call flint_dll_name,$(d))) flint/$(LIBGCC_DLL) flint/$(LIBSTDCXX_DLL) flint/$(LIBWINPTHREAD_DLL) flint/flint.jar $(foreach j,$(JARS),$(call flint_jar_name,$(j))) flint/Makefile flint/header.mk flint/flint.ico flint/flint.xml flint/flint.wxs flint/icon.png flint/license.rtf flint/third-party-licenses.txt
	zip -r flint flint

.DEFAULT_GOAL := all
