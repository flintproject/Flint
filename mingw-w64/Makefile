# -*- Mode: makefile-gmake; tab-width: 4; indent-tabs-mode: t -*-
include config.mk
include ../deposit.mk
include header.mk

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

define flint_boost_dll
$(call flint_dll_name,$(1)): opt/lib/$(1).dll | flint
	install -p $$< $$@
endef

define flint_sundials_dll
$(call flint_dll_name,$(1)): opt/lib/$(1) | flint
	install -p $$< $$@
endef

define flint_exe
$(call flint_exe_name,$(1)): opt/bin/$(1).exe | flint
	install -p $$< $$@
	strip $$@
endef

.PHONY: all mostlyclean clean distclean

all: flint.zip

mostlyclean:
	-rm -rf flint.zip flint var/flint ../configure
	ant -f ../flint/build.xml clean

clean: mostlyclean
	-rm -rf opt usr var/$(BOOST_UNDERSCORE) var/clibsedml-$(CLIBSEDML_COMMIT) var/libsbml-$(LIBSBML_VERSION) var/libxml2-$(LIBXML2_VERSION) var/libzip-$(LIBZIP_VERSION) var/protobuf-$(PROTOBUF_VERSION) var/SBML_odeSolver-$(SOSLIB_COMMIT) var/sundials-$(SUNDIALS_VERSION) var/zlib-$(ZLIB_VERSION) tmp/*.installed tmp/$(BOOST_UNDERSCORE) tmp/clibsedml-$(CLIBSEDML_COMMIT) tmp/libsbml-$(LIBSBML_VERSION) tmp/libxml2-$(LIBXML2_VERSION) tmp/libzip-$(LIBZIP_VERSION) tmp/protobuf-$(PROTOBUF_VERSION) tmp/SBML_odeSolver-$(SOSLIB_COMMIT) tmp/sundials-$(SUNDIALS_VERSION) tmp/zlib-$(ZLIB_VERSION) build/protobuf-$(PROTOBUF_VERSION)

distclean: clean
	-rm -f src/*.zip src/*.tar.bz2 src/*.tar.gz

build var:
	install -d $@

var/toolchain.cmake: toolchain.cmake.in | var
	echo 'set(COMPILER_PREFIX "$(HOST)")' > $@
	cat $< >> $@

tmp: | var
	install -d $@

tmp/$(BOOST_UNDERSCORE): src/$(BOOST_UNDERSCORE).tar.bz2 | tmp
	-rm -rf var/$(BOOST_UNDERSCORE)
	tar -C var -jxf $<
	touch $@

tmp/clibsedml-$(CLIBSEDML_COMMIT): src/clibsedml-$(CLIBSEDML_COMMIT).tar.gz | tmp
	-rm -rf var/clibsedml-$(CLIBSEDML_COMMIT)
	tar -C var -zxf $<
	touch $@

tmp/libsbml-$(LIBSBML_VERSION): src/libSBML-$(LIBSBML_VERSION)-core-src.zip | tmp
	-rm -rf var/libsbml-$(LIBSBML_VERSION)
	(cd var && unzip -qq ../$<)
	touch $@

tmp/libxml2-$(LIBXML2_VERSION): src/libxml2-sources-$(LIBXML2_VERSION).tar.gz | tmp
	-rm -rf var/libxml2-$(LIBXML2_VERSION)
	tar -C var -zxf $<
	touch $@

tmp/libzip-$(LIBZIP_VERSION): src/libzip-$(LIBZIP_VERSION).tar.gz | tmp
	-rm -rf var/libzip-$(LIBZIP_VERSION)
	tar -C var -zxf $<
	touch $@

tmp/zlib-$(ZLIB_VERSION): src/zlib-$(ZLIB_VERSION).tar.gz | tmp
	-rm -rf var/zlib-$(ZLIB_VERSION)
	tar -C var -zxf $<
	touch $@

tmp/protobuf-$(PROTOBUF_VERSION): src/protobuf-$(PROTOBUF_VERSION).tar.gz | tmp
	-rm -rf var/protobuf-$(PROTOBUF_VERSION)
	tar -C var -zxf $<
	touch $@

tmp/SBML_odeSolver-$(SOSLIB_COMMIT): src/SBML_odeSolver-$(SOSLIB_COMMIT).tar.gz | tmp
	-rm -rf var/SBML_odeSolver-$(SOSLIB_COMMIT)
	tar -C var -zxf $<
	touch $@

tmp/sundials-$(SUNDIALS_VERSION): src/sundials-$(SUNDIALS_VERSION).tar.gz sundials-$(SUNDIALS_VERSION).patch | tmp
	-rm -rf var/sundials-$(SUNDIALS_VERSION)
	tar -C var -zxf $<
	patch -d var/sundials-$(SUNDIALS_VERSION) < sundials-$(SUNDIALS_VERSION).patch
	touch $@

tmp/$(BOOST_UNDERSCORE).installed: tmp/$(BOOST_UNDERSCORE)
	(cd var/$(BOOST_UNDERSCORE) && ./bootstrap.sh --prefix=$(PREFIX) --with-toolset=gcc --without-icu)
	echo "using gcc : : $(HOST)-g++" > var/$(BOOST_UNDERSCORE)/user-config.jam
	echo "        :" >> var/$(BOOST_UNDERSCORE)/user-config.jam
	echo "        <rc>$(HOST)-windres" >> var/$(BOOST_UNDERSCORE)/user-config.jam
	echo ";" >> var/$(BOOST_UNDERSCORE)/user-config.jam
	(cd var/$(BOOST_UNDERSCORE) && ./b2 -d0 --prefix=$(PREFIX) --without-atomic --without-context --without-coroutine --without-coroutine2 --without-log --without-mpi --without-python --without-thread --without-wave --layout=tagged --user-config=user-config.jam -j 2 -sNO_BZIP2=1 -q toolset=gcc target-os=windows variant=release threading=multi threadapi=pthread link=shared runtime-link=shared install)
	touch $@

tmp/clibsedml-$(CLIBSEDML_COMMIT).installed: tmp/clibsedml-$(CLIBSEDML_COMMIT) tmp/libxml2-$(LIBXML2_VERSION).installed
	(cd var/clibsedml-$(CLIBSEDML_COMMIT) && autoreconf -i -f && env CFLAGS='-std=c99 -Wall -W -Werror' ./configure --host=$(HOST) --prefix=$(PREFIX) --with-xml-prefix=$(PREFIX) --disable-xmltest && $(MAKE) all install)
	touch $@

usr/bin/protoc: src/protobuf-$(PROTOBUF_VERSION).tar.gz | build
	-rm -rf build/protobuf-$(PROTOBUF_VERSION)
	tar -C build -zxf $<
	(cd build/protobuf-$(PROTOBUF_VERSION) && ./configure --prefix=$(abspath usr) && $(MAKE) all install)

tmp/protobuf-$(PROTOBUF_VERSION).installed: tmp/protobuf-$(PROTOBUF_VERSION) | usr/bin/protoc
	(cd var/protobuf-$(PROTOBUF_VERSION) && ./configure --host=$(HOST) --prefix=$(PREFIX) --with-protoc=$(abspath usr/bin/protoc) --enable-shared --disable-static)
	$(MAKE) -C var/protobuf-$(PROTOBUF_VERSION) all install
	touch $@

tmp/libxml2-$(LIBXML2_VERSION).installed: tmp/libxml2-$(LIBXML2_VERSION)
	(cd var/libxml2-$(LIBXML2_VERSION) && ./configure --host=$(HOST) --prefix=$(PREFIX) --without-python --without-threads && $(MAKE) all install)
	touch $@

tmp/libzip-$(LIBZIP_VERSION).installed: tmp/libzip-$(LIBZIP_VERSION) tmp/zlib-$(ZLIB_VERSION).installed
	(cd var/libzip-$(LIBZIP_VERSION) && ./configure --host=$(HOST) --prefix=$(PREFIX) --with-zlib=$(PREFIX) && $(MAKE) all install)
	touch $@

tmp/sundials-$(SUNDIALS_VERSION).installed: tmp/sundials-$(SUNDIALS_VERSION)
	(cd var/sundials-$(SUNDIALS_VERSION) && ./configure --host=$(HOST) --prefix=$(PREFIX) --enable-shared --disable-static --disable-mpi)
	$(MAKE) -C var/sundials-$(SUNDIALS_VERSION) all install LDFLAGS='-no-undefined' LIBS='' # to drop -lm
	touch $@

tmp/libsbml-$(LIBSBML_VERSION).installed: var/toolchain.cmake tmp/libsbml-$(LIBSBML_VERSION) tmp/libxml2-$(LIBXML2_VERSION).installed
	mkdir -p var/libsbml-$(LIBSBML_VERSION)/build
	(cd var/libsbml-$(LIBSBML_VERSION)/build && cmake --host=$(HOST) -DCMAKE_INSTALL_PREFIX=$(PREFIX) -DCMAKE_TOOLCHAIN_FILE=$(CURDIR)/var/toolchain.cmake -DLIBXML_INCLUDE_DIR=$(PREFIX)/include/libxml2 -DLIBXML_LIBRARY=$(PREFIX)/bin/libxml2-2.dll ..)
	$(MAKE) -C var/libsbml-$(LIBSBML_VERSION)/build all install
	touch $@

tmp/SBML_odeSolver-$(SOSLIB_COMMIT).installed: tmp/SBML_odeSolver-$(SOSLIB_COMMIT) tmp/sundials-$(SUNDIALS_VERSION).installed tmp/libsbml-$(LIBSBML_VERSION).installed
	(cd var/SBML_odeSolver-$(SOSLIB_COMMIT) && autoreconf -i -f && ./configure --host=$(HOST) --prefix=$(PREFIX) --enable-shared --disable-static --with-libsbml=$(PREFIX) --with-libxml2=$(PREFIX) --without-xerces --with-sundials=$(PREFIX) --without-grace --without-graphviz CPPFLAGS='-DSBML_ODESOLVER_EXPORTS=1 -DWIN32')
	-$(MAKE) -C var/SBML_odeSolver-$(SOSLIB_COMMIT) install LDFLAGS='-no-undefined'
	touch $@

tmp/zlib-$(ZLIB_VERSION).installed: tmp/zlib-$(ZLIB_VERSION)
	(cd var/zlib-$(ZLIB_VERSION) && $(MAKE) -fwin32/Makefile.gcc all install PREFIX=$(HOST)- DESTDIR=$(PREFIX)/ INCLUDE_PATH=include LIBRARY_PATH=lib BINARY_PATH=bin SHARED_MODE=1)
	touch $@

../flint/dist/flint.jar: $(foreach e,$(EXES),opt/bin/$(e).exe)
	ant -f ../flint/build.xml jar

../configure: ../configure.ac
	(cd .. && autoreconf -i -f)

flint opt usr var/flint:
	install -d $@

flint/lib: | flint
	install -d $@

var/flint/Makefile: tmp/$(BOOST_UNDERSCORE).installed tmp/clibsedml-$(CLIBSEDML_COMMIT).installed tmp/protobuf-$(PROTOBUF_VERSION).installed tmp/libzip-$(LIBZIP_VERSION).installed tmp/SBML_odeSolver-$(SOSLIB_COMMIT).installed ../configure | usr/bin/protoc var/flint
	(cd var/flint && env CPPFLAGS='-DWIN32_LEAN_AND_MEAN -D_WIN32_WINNT=0x0600 -DWIN32 -DSBML_ODESOLVER_EXPORTS=1' LIBS='-lole32 -loleaut32 -lmswsock -lws2_32' PKG_CONFIG_PATH=$(PKG_CONFIG_PATH) LIBSBML_CFLAGS=$(PREFIX)/include LIBSBML_LIBS='-lsbml' SOSLIB_CFLAGS=$(PREFIX)/include SOSLIB_LIBS='-lODES' ../../../configure --host=$(HOST) --prefix=$(PREFIX) --with-boost=$(PREFIX) --with-xml-prefix=$(PREFIX) --disable-xmltest --with-protoc=$(abspath usr/bin/protoc) --with-sundials=$(PREFIX) --enable-shared --disable-static)

$(foreach d,$(FLINT_DLLS),opt/bin/$(d).dll) $(foreach e,$(EXES),opt/bin/$(e).exe): $(foreach d,$(BIN_DLLS) $(BOOST_DLLS),$(call flint_dll_name,$(d))) tmp/sundials-$(SUNDIALS_VERSION).installed tmp/SBML_odeSolver-$(SOSLIB_COMMIT).installed var/flint/Makefile
	$(MAKE) -C var/flint all install-exec

flint/flint.jar: ../flint/dist/flint.jar | flint
	install -p -m 0644 $< $@

$(foreach j,$(JARS),$(eval $(call flint_jar,$(j))))

opt/bin/libODES-0.dll: tmp/SBML_odeSolver-$(SOSLIB_COMMIT).installed

opt/bin/libprotobuf-9.dll: tmp/protobuf-$(PROTOBUF_VERSION).installed

opt/bin/libsedml-0.dll: tmp/clibsedml-$(CLIBSEDML_COMMIT).installed

opt/bin/libsbml.all: tmp/libsbml-$(LIBSBML_VERSION).installed

opt/bin/libxml2-2.dll: tmp/libxml2-$(LIBXML2_VERSION).installed

opt/bin/libzip-2.dll: tmp/libzip-$(LIBZIP_VERSION).installed

opt/bin/zlib1.dll: tmp/zlib-$(ZLIB_VERSION).installed

$(foreach d,$(BIN_DLLS) $(FLINT_DLLS),$(eval $(call flint_bin_dll,$(d))))

$(foreach d,$(BOOST_DLLS),opt/lib/$(d).dll): tmp/$(BOOST_UNDERSCORE).installed

$(foreach d,$(BOOST_DLLS),$(eval $(call flint_boost_dll,$(d))))

$(foreach d,$(SUNDIALS_DLLS),$(eval $(call flint_sundials_dll,$(d))))

$(foreach d,$(SUNDIALS_DLLS),opt/lib/$(d)): tmp/sundials-$(SUNDIALS_VERSION).installed

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
	m4 -DLIBGCC_DLL=$(LIBGCC_DLL) -DLIBSTDCXX_DLL=$(LIBSTDCXX_DLL) -DLIBWINPTHREAD_DLL=$(LIBWINPTHREAD_DLL) $< > $@

flint/icon.png: ../flint/src/jp/oist/flint/image/icon.png | flint
	install -p -m 0644 $< $@

flint/license.rtf: ../doc/license.rtf | flint
	install -p -m 0644 $< $@

flint/third-party-licenses.txt: tmp/libsbml-$(LIBSBML_VERSION) tmp/libxml2-$(LIBXML2_VERSION) tmp/libzip-$(LIBZIP_VERSION) tmp/protobuf-$(PROTOBUF_VERSION) tmp/SBML_odeSolver-$(SOSLIB_COMMIT) tmp/sundials-$(SUNDIALS_VERSION) | flint
	echo ';;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;' > $@
	echo ';; Licenses of third-party libraries' >> $@
	echo ';;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;' >> $@
	echo >> $@
	echo ';; libSBML $(LIBSBML_VERSION)' >> $@
	echo >> $@
	cat var/libsbml-$(LIBSBML_VERSION)/LICENSE.txt >> $@
	echo >> $@
	echo ';; libxml2 $(LIBXML2_VERSION)' >> $@
	echo >> $@
	cat var/libxml2-$(LIBXML2_VERSION)/Copyright >> $@
	echo >> $@
	echo ';; libzip $(LIBZIP_VERSION)' >> $@
	echo >> $@
	cat var/libzip-$(LIBZIP_VERSION)/LICENSE >> $@
	echo >> $@
	echo ';; Protocol Buffers $(PROTOBUF_VERSION)' >> $@
	echo >> $@
	cat var/protobuf-$(PROTOBUF_VERSION)/LICENSE >> $@
	echo >> $@
	echo ';; SOSlib' >> $@
	echo >> $@
	cat var/SBML_odeSolver-$(SOSLIB_COMMIT)/COPYING >> $@
	echo >> $@
	echo ';; SUNDIALS $(SUNDIALS_VERSION)' >> $@
	echo >> $@
	cat var/sundials-$(SUNDIALS_VERSION)/LICENSE >> $@

flint.zip: $(foreach e,$(EXES),$(call flint_exe_name,$(e))) $(foreach d,$(BOOST_DLLS) $(SUNDIALS_DLLS) $(FLINT_DLLS),$(call flint_dll_name,$(d))) flint/$(LIBGCC_DLL) flint/$(LIBSTDCXX_DLL) flint/$(LIBWINPTHREAD_DLL) flint/flint.jar $(foreach j,$(JARS),$(call flint_jar_name,$(j))) flint/Makefile flint/header.mk flint/flint.ico flint/flint.xml flint/flint.wxs flint/icon.png flint/license.rtf flint/third-party-licenses.txt
	zip -r flint flint

.DEFAULT_GOAL := all
