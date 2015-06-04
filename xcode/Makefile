# -*- Mode: makefile-gmake; tab-width: 4; indent-tabs-mode: t -*-
include config.mk
include ../deposit.mk

PREFIX = $(abspath usr)

PKG_CONFIG_PATH = $(PREFIX)/lib/pkgconfig

JARS := \
    GarudaBackend \
    commons-cli-1.2 \
    commons-logging-1.1.3 \
    commons-vfs2-2.0 \
    dom4j-1.6.1 \
    garuda-csr \
    jcommon-1.0.22 \
    jfreechart-1.0.18 \
    jsonic-1.2.10 \
    log4j-1.2.16 \
    protobuf-java-$(PROTOBUF_VERSION) \
    sqlite-jdbc-3.8.6
BOOST_DYLIBS := \
    filesystem \
    program_options \
    system
DYLIBS := \
    libODES.0 \
    libsbml.$(LIBSBML_VERSION) \
    libsedml.0 \
    libxml2.2 \
    libprotobuf.9 \
    libzip.2
SUNDIALS_DYLIBS := \
    cvode.0 \
    cvodes.1 \
    ida.1 \
    kinsol.0 \
    nvecserial.0
FLINT_DYLIBS := \
    libflint.0 \
    libflintxx.0 \
    libsqlite3.0
PROGRAMS := \
    csv2isd \
    flint-compile \
    flint-concat \
    flint-cut \
    flint-enum \
    flint-evolve \
    flint-exec \
    flint-file \
    flint-filter \
    flint-form \
    flint-generate \
    flint-init \
    flint-isdh \
    flint-lands \
    flint-layout \
    flint-loadconfig \
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
    isddiff \
    isdplot \
    isdsort \
    isdstrip \
    isdwc
EXT_PROGRAMS = flint-make

DMGS = flint.dmg

APP = root/Applications/Flint.app

flint_java_jar_name = $(APP)/Contents/Java/$(1).jar
flint_frameworks_boost_dylib_name = $(APP)/Contents/Frameworks/libboost_$(1)-mt.dylib
flint_frameworks_dylib_name = $(APP)/Contents/Frameworks/$(1).dylib
flint_frameworks_sundials_dylib_name = $(APP)/Contents/Frameworks/libsundials_$(1)
flint_macos_program_name = $(APP)/Contents/MacOS/$(1)

define external_library_source_for_osx
src/$(1):
	curl $(DEPOSIT_URL)/$(1) -o $$@
	md5 $$@ | grep $(2)
endef

define flint_frameworks_boost_dylib
$(call flint_frameworks_boost_dylib_name,$(1)): tmp/$(BOOST_UNDERSCORE).installed tmp/bundled
	install -p -m 0644 $(PREFIX)/lib/libboost_$(1)-mt.dylib $$@
	install_name_tool -id @executable_path/../Frameworks/libboost_$(1)-mt.dylib $$@
	install_name_tool -change libboost_system-mt.dylib @executable_path/../Frameworks/libboost_system-mt.dylib $$@
endef

define flint_frameworks_dylib
$(call flint_frameworks_dylib_name,$(1)): $(PREFIX)/lib/$(1).dylib tmp/bundled
	install -p -m 0644 $$< $$@
	install_name_tool -id @executable_path/../Frameworks/$(1).dylib $$@
	install_name_tool -change $(PREFIX)/lib/libxml2.2.dylib @executable_path/../Frameworks/libxml2.2.dylib $$@
	install_name_tool -change $(PREFIX)/lib/libsbml.$(LIBSBML_VERSION).dylib @executable_path/../Frameworks/libsbml.$(LIBSBML_VERSION).dylib $$@
	for name in $(SUNDIALS_DYLIBS); do install_name_tool -change $(PREFIX)/lib/libsundials_$$$$name @executable_path/../Frameworks/libsundials_$$$$name $$@; done
endef

define flint_frameworks_sundials_dylib
$(call flint_frameworks_sundials_dylib_name,$(1)): tmp/sundials-$(SUNDIALS_VERSION).installed tmp/bundled
	install -p -m 0644 $(PREFIX)/lib/libsundials_$(1) $$@
	install_name_tool -id @executable_path/../Frameworks/libsundials_$(1) $$@
endef

define flint_frameworks_flint_dylib
$(call flint_frameworks_dylib_name,$(1)): opt/lib/$(1).dylib tmp/bundled
	install -p -m 0644 $$< $$@
	install_name_tool -id @executable_path/../Frameworks/$(1).dylib $$@
	for name in $(BOOST_DYLIBS); do install_name_tool -change libboost_$$$$name-mt.dylib @executable_path/../Frameworks/libboost_$$$$name-mt.dylib $$@; done
	for name in $(DYLIBS); do install_name_tool -change $(PREFIX)/lib/$$$$name.dylib @executable_path/../Frameworks/$$$$name.dylib $$@; done
	for name in $(SUNDIALS_DYLIBS); do install_name_tool -change $(PREFIX)/lib/libsundials_$$$$name @executable_path/../Frameworks/libsundials_$$$$name $$@; done
	for name in $(FLINT_DYLIBS); do install_name_tool -change $(CURDIR)/opt/lib/$$$$name.dylib @executable_path/../Frameworks/$$$$name.dylib $$@; done
endef

define flint_macos_program
$(call flint_macos_program_name,$(1)): opt/bin/$(1) tmp/bundled
	install -p $$< $$@
	for name in $(BOOST_DYLIBS); do install_name_tool -change libboost_$$$$name-mt.dylib @executable_path/../Frameworks/libboost_$$$$name-mt.dylib $$@; done
	for name in $(DYLIBS); do install_name_tool -change $(PREFIX)/lib/$$$$name.dylib @executable_path/../Frameworks/$$$$name.dylib $$@; done
	for name in $(SUNDIALS_DYLIBS); do install_name_tool -change $(PREFIX)/lib/libsundials_$$$$name @executable_path/../Frameworks/libsundials_$$$$name $$@; done
	for name in $(FLINT_DYLIBS); do install_name_tool -change $(CURDIR)/opt/lib/$$$$name.dylib @executable_path/../Frameworks/$$$$name.dylib $$@; done
endef

define flint_ext_program
$(call flint_macos_program_name,$(1)): usr/bin/$(1) tmp/bundled
	install $$< $$@
endef

.PHONY: all check mostlyclean clean distclean

all: $(foreach lib,$(BOOST_UNDERSCORE) clibsedml-$(CLIBSEDML_COMMIT) libsbml-$(LIBSBML_VERSION) libxml2-$(LIBXML2_VERSION) libzip-$(LIBZIP_VERSION) protobuf-$(PROTOBUF_VERSION) SBML_odeSolver-$(SOSLIB_COMMIT) sundials-$(SUNDIALS_VERSION),tmp/$(lib).installed) $(DMGS)

check:
	env DYLD_LIBRARY_PATH=$(PREFIX)/lib $(MAKE) -C build check

mostlyclean:
	-rm -rf tmp/bundled $(DMGS) flint flint-component.pkg resources root opt build ../configure
	ant -f ../flint/build.xml clean

clean: mostlyclean
	-rm -rf usr tmp/make-3.82 var/make-3.82 $(foreach lib,$(EXTERNAL_LIBRARY_DIRS),tmp/$(lib).installed tmp/$(lib) var/$(lib))

distclean: clean
	-rm -f $(foreach lib,make-3.82.tar.gz $(EXTERNAL_LIBRARIES),src/$(lib))

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
	(cd var && unzip ../$<)
	touch $@

tmp/libxml2-$(LIBXML2_VERSION): src/libxml2-sources-$(LIBXML2_VERSION).tar.gz | tmp
	-rm -rf var/libxml2-$(LIBXML2_VERSION)
	tar -C var -zxf $<
	touch $@

tmp/libzip-$(LIBZIP_VERSION): src/libzip-$(LIBZIP_VERSION).tar.gz | tmp
	-rm -rf var/libzip-$(LIBZIP_VERSION)
	tar -C var -zxf $<
	touch $@

tmp/make-3.82: src/make-3.82.tar.gz | tmp
	-rm -rf var/make-3.82
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

tmp/sundials-$(SUNDIALS_VERSION): src/sundials-$(SUNDIALS_VERSION).tar.gz | tmp
	-rm -rf var/sundials-$(SUNDIALS_VERSION)
	tar -C var -zxf $<
	touch $@

tmp/$(BOOST_UNDERSCORE).installed: tmp/$(BOOST_UNDERSCORE)
	(cd var/$(BOOST_UNDERSCORE) && export MACOSX_DEPLOYMENT_TARGET=10.7 && ./bootstrap.sh --with-toolset=darwin && ./b2 -d0 --prefix=$(PREFIX) --without-mpi --without-python --layout=tagged toolset=darwin cflags='-arch x86_64 -mmacosx-version-min=10.7' cxxflags='-arch x86_64 -mmacosx-version-min=10.7' linkflags='-arch x86_64 -mmacosx-version-min=10.7' link=shared variant=release threading=multi install)
	touch $@

tmp/clibsedml-$(CLIBSEDML_COMMIT).installed: tmp/clibsedml-$(CLIBSEDML_COMMIT) tmp/libxml2-$(LIBXML2_VERSION).installed
	(cd var/clibsedml-$(CLIBSEDML_COMMIT) && autoreconf -i -f && export MACOSX_DEPLOYMENT_TARGET=10.7 && env CFLAGS='-Wall -W -arch x86_64 -mmacosx-version-min=10.7' LDFLAGS='-arch x86_64 -mmacosx-version-min=10.7' ./configure --prefix=$(PREFIX) --with-xml-prefix=$(PREFIX) && $(MAKE) all install)
	touch $@

tmp/protobuf-$(PROTOBUF_VERSION).installed: tmp/protobuf-$(PROTOBUF_VERSION)
	(cd var/protobuf-$(PROTOBUF_VERSION) && export MACOSX_DEPLOYMENT_TARGET=10.7 && env CFLAGS='-arch x86_64 -mmacosx-version-min=10.7' CXXFLAGS='-arch x86_64 -mmacosx-version-min=10.7' LDFLAGS='-arch x86_64 -mmacosx-version-min=10.7' ./configure --prefix=$(PREFIX) && $(MAKE) all install)
	touch $@

tmp/libxml2-$(LIBXML2_VERSION).installed: tmp/libxml2-$(LIBXML2_VERSION)
	(cd var/libxml2-$(LIBXML2_VERSION) && export MACOSX_DEPLOYMENT_TARGET=10.7 && env CFLAGS='-arch x86_64 -mmacosx-version-min=10.7' CXXFLAGS='-arch x86_64 -mmacosx-version-min=10.7' LDFLAGS='-arch x86_64 -mmacosx-version-min=10.7' ./configure --prefix=$(PREFIX) --without-python && $(MAKE) all install)
	touch $@

tmp/libzip-$(LIBZIP_VERSION).installed: tmp/libzip-$(LIBZIP_VERSION)
	(cd var/libzip-$(LIBZIP_VERSION) && export MACOSX_DEPLOYMENT_TARGET=10.7 && env CFLAGS='-arch x86_64 -mmacosx-version-min=10.7' CXXFLAGS='-arch x86_64 -mmacosx-version-min=10.7' LDFLAGS='-arch x86_64 -mmacosx-version-min=10.7' ./configure --prefix=$(PREFIX) && $(MAKE) all install)
	touch $@

tmp/sundials-$(SUNDIALS_VERSION).installed: tmp/sundials-$(SUNDIALS_VERSION)
	(cd var/sundials-$(SUNDIALS_VERSION) && export MACOSX_DEPLOYMENT_TARGET=10.7 && env CFLAGS='-arch x86_64 -mmacosx-version-min=10.7' CXXFLAGS='-arch x86_64 -mmacosx-version-min=10.7' LDFLAGS='-arch x86_64 -mmacosx-version-min=10.7' ./configure --prefix=$(PREFIX) --enable-shared && $(MAKE) all install)
	touch $@

tmp/libsbml-$(LIBSBML_VERSION).installed: tmp/libsbml-$(LIBSBML_VERSION) tmp/libxml2-$(LIBXML2_VERSION).installed
	(cd var/libsbml-$(LIBSBML_VERSION) && export MACOSX_DEPLOYMENT_TARGET=10.7 && env CFLAGS='-arch x86_64 -mmacosx-version-min=10.7' CXXFLAGS='-arch x86_64 -mmacosx-version-min=10.7' LDFLAGS='-arch x86_64 -mmacosx-version-min=10.7' ./configure --prefix=$(PREFIX) --with-xerces=no --with-libxml=$(PREFIX) && $(MAKE) all install)
	install_name_tool -id $(PREFIX)/lib/libsbml.$(LIBSBML_VERSION).dylib $(PREFIX)/lib/libsbml.$(LIBSBML_VERSION).dylib
	touch $@

tmp/SBML_odeSolver-$(SOSLIB_COMMIT).installed: tmp/SBML_odeSolver-$(SOSLIB_COMMIT) tmp/sundials-$(SUNDIALS_VERSION).installed tmp/libsbml-$(LIBSBML_VERSION).installed
	(cd var/SBML_odeSolver-$(SOSLIB_COMMIT) && autoreconf -i && export MACOSX_DEPLOYMENT_TARGET=10.7 && env CFLAGS='-arch x86_64 -mmacosx-version-min=10.7' CXXFLAGS='-arch x86_64 -mmacosx-version-min=10.7' LDFLAGS='-arch x86_64 -mmacosx-version-min=10.7' ./configure --prefix=$(PREFIX) --with-libsbml=$(PREFIX) --with-libxml2=$(PREFIX) --with-sundials=$(PREFIX) --without-grace --without-graphviz && $(MAKE) all install)
	touch $@

usr/bin/flint-make: tmp/make-3.82
	(cd var/make-3.82 && export MACOSX_DEPLOYMENT_TARGET=10.7 && ./configure --prefix=$(PREFIX) --program-prefix=flint- CFLAGS='-arch x86_64 -mmacosx-version-min=10.7' LDFLAGS='-arch x86_64 -mmacosx-version-min=10.7' && $(MAKE) all install)

# for Flint

../flint/dist/flint.jar: $(foreach p,$(PROGRAMS),opt/bin/$(p))
	ant -f ../flint/build.xml jar

../configure: ../configure.ac
	(cd .. && autoreconf -i -f)

build/Makefile: ../configure
	install -d $(dir $@)
	(cd $(dir $@) && env CFLAGS='-arch x86_64 -mmacosx-version-min=10.7' CXXFLAGS='-arch x86_64 -mmacosx-version-min=10.7' LDFLAGS='-arch x86_64 -mmacosx-version-min=10.7 -headerpad_max_install_names' PKG_CONFIG=$(PKG_CONFIG) PKG_CONFIG_PATH=$(PKG_CONFIG_PATH) ../../configure --prefix=$(CURDIR)/opt --with-boost=$(PREFIX) --with-protoc=$(PREFIX)/bin/protoc --with-xml-prefix=$(PREFIX) --with-protoc=$(PREFIX)/bin/protoc --with-sundials=$(PREFIX))

$(foreach p,$(PROGRAMS),opt/bin/$(p)): build/Makefile
	$(MAKE) -C build all install-exec

$(foreach d,$(BOOST_DYLIBS),$(eval $(call flint_frameworks_boost_dylib,$(d))))

$(foreach d,$(DYLIBS),$(eval $(call flint_frameworks_dylib,$(d))))

$(foreach d,$(SUNDIALS_DYLIBS),$(eval $(call flint_frameworks_sundials_dylib,$(d))))

$(foreach d,$(FLINT_DYLIBS),$(eval $(call flint_frameworks_flint_dylib,$(d))))

root:
	install -d $@

root/Applications: | root
	install -d $@

tmp/bundled: Info.plist.patch build.xml ../flint/dist/flint.jar ../flint.icns | root/Applications
	ant bundleapp
	patch $(APP)/Contents/Info.plist $<
	mkdir $(APP)/Contents/Frameworks/
	touch $@

$(APP)/Contents/Info.plist $(APP)/Contents/Java/flint.jar $(foreach j,$(JARS),$(call flint_java_jar_name,$(j))) $(APP)/Contents/MacOS/JavaAppLauncher $(APP)/Contents/PkgInfo $(APP)/Contents/Resources/flint.icns: tmp/bundled

$(foreach p,$(PROGRAMS),$(eval $(call flint_macos_program,$(p))))

$(eval $(call flint_ext_program,flint-make))

$(APP)/Contents/Resources/third-party-licenses.txt: tmp/libsbml-$(LIBSBML_VERSION) tmp/libxml2-$(LIBXML2_VERSION) tmp/protobuf-$(PROTOBUF_VERSION) tmp/SBML_odeSolver-$(SOSLIB_COMMIT) tmp/sundials-$(SUNDIALS_VERSION)
	install -d $(dir $@)
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

$(APP)/Contents/Resources/icon.png: ../flint/src/jp/oist/flint/image/icon.png
	install -d $(dir $@)
	install -p -m 0644 $< $@

flint-component.pkg: component.plist $(foreach d,$(BOOST_DYLIBS),$(call flint_frameworks_boost_dylib_name,$(d))) $(foreach d,$(DYLIBS) $(FLINT_DYLIBS),$(call flint_frameworks_dylib_name,$(d))) $(foreach d,$(SUNDIALS_DYLIBS),$(call flint_frameworks_sundials_dylib_name,$(d))) $(foreach p,$(PROGRAMS) $(EXT_PROGRAMS),$(call flint_macos_program_name,$(p))) $(foreach j,$(JARS),$(call flint_java_jar_name,$(j))) $(APP)/Contents/Resources/third-party-licenses.txt $(APP)/Contents/Resources/flint.icns $(APP)/Contents/Resources/icon.png $(APP)/Contents/Java/flint.jar $(APP)/Contents/MacOS/JavaAppLauncher $(APP)/Contents/Info.plist $(APP)/Contents/PkgInfo
	pkgbuild --quiet --identifier jp.oist.flint --component-plist $< --root root $@

flint resources:
	install -d $@

resources/license.rtf: ../doc/license.rtf | resources
	install -p -m 0644 $< $@

flint/flint.pkg: distribution.xml flint-component.pkg resources/license.rtf | flint
	productbuild --quiet --distribution $< --resources resources $@

flint.dmg: flint/flint.pkg
	hdiutil create -fs HFS+ -ov -srcfolder flint $@
	install -p -m 0644 $@ flint-`date +%Y%m%d`.dmg

$(eval $(call external_library_source_for_osx,make-3.82.tar.gz,7f7c000e3b30c6840f2e9cf86b254fac))

.DEFAULT_GOAL := all
