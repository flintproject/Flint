-include config.mk
include ../deposit.mk

PREFIX ?= $(abspath opt)

PKG_CONFIG_PATH = $(PREFIX)/clibsedml-$(CLIBSEDML_COMMIT)/lib/pkgconfig:$(PREFIX)/libsbml-$(LIBSBML_VERSION)/lib/pkgconfig:$(PREFIX)/libxml2-$(LIBXML2_VERSION)/lib/pkgconfig:$(PREFIX)/libzip-$(LIBZIP_VERSION)/lib/pkgconfig:$(PREFIX)/protobuf-$(PROTOBUF_VERSION)/lib/pkgconfig:$(PREFIX)/SBML_odeSolver-$(SOSLIB_COMMIT)/lib/pkgconfig

.PHONY: all check check-sequential-version download mostlyclean clean distclean install

all: download tmp/sequential-version.installed

check: check-sequential-version

check-sequential-version: tmp/sequential-version.installed
	$(MAKE) -C var/sequential-version check

download: $(foreach lib,$(EXTERNAL_LIBRARIES),src/$(lib))

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

tmp/protobuf-$(PROTOBUF_VERSION): src/protobuf-java-$(PROTOBUF_VERSION).tar.gz | tmp
	-rm -rf var/protobuf-$(PROTOBUF_VERSION)
	tar -C var -zxf $<
	touch $@

tmp/SBML_odeSolver-$(SOSLIB_COMMIT): src/SBML_odeSolver-$(SOSLIB_COMMIT).tar.gz | tmp
	-rm -rf var/SBML_odeSolver-$(SOSLIB_COMMIT)
	tar -C var -zxf $<
	touch $@

tmp/sundials-$(SUNDIALS_VERSION): src/sundials-$(SUNDIALS_VERSION).tar.gz | tmp
	-rm -rf src/sundials-$(SUNDIALS_VERSION)
	tar -C src -zxf $<
	touch $@

tmp/wxWidgets-$(WXWIDGETS_VERSION): src/wxWidgets-$(WXWIDGETS_VERSION).tar.bz2 | tmp
	-rm -rf src/wxWidgets-$(WXWIDGETS_VERSION)
	tar -C var -jxf $<
	touch $@

tmp/$(BOOST_UNDERSCORE).installed: tmp/$(BOOST_UNDERSCORE)
	-(cd var/$(BOOST_UNDERSCORE) && ./bootstrap.sh --prefix=$(PREFIX)/$(BOOST_UNDERSCORE) && ./b2 --layout=tagged --without-atomic --without-chrono --without-container --without-mpi --without-python --without-serialization --without-type_erasure install)
	touch $@

tmp/clibsedml-$(CLIBSEDML_COMMIT).installed: tmp/clibsedml-$(CLIBSEDML_COMMIT) tmp/libxml2-$(LIBXML2_VERSION).installed
	(cd var/clibsedml-$(CLIBSEDML_COMMIT) && autoreconf -i -f && ./configure --prefix=$(PREFIX)/clibsedml-$(CLIBSEDML_COMMIT) --with-xml-prefix=$(PREFIX)/libxml2-$(LIBXML2_VERSION) && $(MAKE) all install)
	touch $@

tmp/protobuf-$(PROTOBUF_VERSION).installed: tmp/protobuf-$(PROTOBUF_VERSION)
	(cd var/protobuf-$(PROTOBUF_VERSION) && ./configure --prefix=$(PREFIX)/protobuf-$(PROTOBUF_VERSION) && $(MAKE) all install)
	touch $@

tmp/libxml2-$(LIBXML2_VERSION).installed: tmp/libxml2-$(LIBXML2_VERSION)
	(cd var/libxml2-$(LIBXML2_VERSION) && ./configure --prefix=$(PREFIX)/libxml2-$(LIBXML2_VERSION) --without-python && $(MAKE) all install)
	touch $@

tmp/libzip-$(LIBZIP_VERSION).installed: tmp/libzip-$(LIBZIP_VERSION)
	(cd var/libzip-$(LIBZIP_VERSION) && ./configure --prefix=$(PREFIX)/libzip-$(LIBZIP_VERSION) && $(MAKE) all install)
	touch $@

tmp/sundials-$(SUNDIALS_VERSION).installed: tmp/sundials-$(SUNDIALS_VERSION)
	-rm -rf var/sundials-$(SUNDIALS_VERSION)
	install -d var/sundials-$(SUNDIALS_VERSION)
	(cd var/sundials-$(SUNDIALS_VERSION) && cmake -DCMAKE_INSTALL_PREFIX=$(PREFIX)/sundials-$(SUNDIALS_VERSION) -DBUILD_STATIC_LIBS=OFF -DBUILD_SHARED_LIBS=ON ../../src/sundials-$(SUNDIALS_VERSION) && $(MAKE) all install)
	touch $@

tmp/wxWidgets-$(WXWIDGETS_VERSION).installed: tmp/wxWidgets-$(WXWIDGETS_VERSION)
	(cd var/wxWidgets-$(WXWIDGETS_VERSION) && ./configure --prefix=$(PREFIX)/wxWidgets-$(WXWIDGETS_VERSION) --enable-cxx11 --disable-compat30 --enable-monolithic && $(MAKE) all install)
	touch $@

tmp/libsbml-$(LIBSBML_VERSION).installed: tmp/libsbml-$(LIBSBML_VERSION) tmp/libxml2-$(LIBXML2_VERSION).installed
	(cd var/libsbml-$(LIBSBML_VERSION) && ./configure --prefix=$(PREFIX)/libsbml-$(LIBSBML_VERSION) --with-xerces=no --with-libxml=$(PREFIX)/libxml2-$(LIBXML2_VERSION) && $(MAKE) all install)
	touch $@

tmp/SBML_odeSolver-$(SOSLIB_COMMIT).installed: tmp/SBML_odeSolver-$(SOSLIB_COMMIT) tmp/sundials-$(SUNDIALS_VERSION).installed tmp/libsbml-$(LIBSBML_VERSION).installed
	(cd var/SBML_odeSolver-$(SOSLIB_COMMIT) && autoreconf -i && ./configure --prefix=$(PREFIX)/SBML_odeSolver-$(SOSLIB_COMMIT) --with-libsbml=$(PREFIX)/libsbml-$(LIBSBML_VERSION) --with-libxml2=$(PREFIX)/libxml2-$(LIBXML2_VERSION) --with-sundials=$(PREFIX)/sundials-$(SUNDIALS_VERSION) --without-grace --without-graphviz && $(MAKE) all install)
	touch $@

tmp/sequential-version.installed: tmp/$(BOOST_UNDERSCORE).installed tmp/clibsedml-$(CLIBSEDML_COMMIT).installed tmp/wxWidgets-$(WXWIDGETS_VERSION).installed tmp/libzip-$(LIBZIP_VERSION).installed tmp/protobuf-$(PROTOBUF_VERSION).installed tmp/SBML_odeSolver-$(SOSLIB_COMMIT).installed
	(cd .. && autoreconf -i -f)
	install -d var/sequential-version
	(cd var/sequential-version && env LDFLAGS='-Wl,-rpath -Wl,$(PREFIX)/libsbml-$(LIBSBML_VERSION)/lib -Wl,-rpath -Wl,$(PREFIX)/sundials-$(SUNDIALS_VERSION)/lib' PKG_CONFIG_PATH=$(PKG_CONFIG_PATH) ../../../configure --prefix=$(PREFIX)/sequential-version --with-boost=$(PREFIX)/$(BOOST_UNDERSCORE) --with-xml-prefix=$(PREFIX)/libxml2-$(LIBXML2_VERSION) --with-protoc=$(PREFIX)/protobuf-$(PROTOBUF_VERSION)/bin/protoc --with-sundials=$(PREFIX)/sundials-$(SUNDIALS_VERSION) --with-wxWidgets=$(PREFIX)/wxWidgets-$(WXWIDGETS_VERSION) --enable-silent-rules && $(MAKE) all install)
	touch $@

.DEFAULT_GOAL := all
