# -*- Mode: makefile-gmake; tab-width: 4; indent-tabs-mode: t -*-
include config.mk
include ../deposit.mk
include ../example.mk
include ../option.mk
include ../version.mk

SQLITE_YEAR = 2022
SQLITE_VSTR = 3370200

$(eval $(call external_library_source,https://sqlite.org/$(SQLITE_YEAR),sqlite-amalgamation-$(SQLITE_VSTR).zip,cb25df0fb90b77be6660f6ace641bbea88f3d0441110d394ce418f35f7561bb0))
$(eval $(call external_library_source,https://sqlite.org/$(SQLITE_YEAR),sqlite-dll-win64-x64-$(SQLITE_VSTR).zip,456bef46dde6e18e66ca6183a210248286b03196a688de57f3345d7df14c808c))

$(eval $(call external_library_source,https://ftp.gnu.org/gnu/libmicrohttpd,libmicrohttpd-$(LIBMICROHTTPD_VERSION)-w32-bin.zip,0f9bd6c17b1c3653d34fa5bbb799b8a8f633c17a6165fb92a1dcc65bff39f046))

$(eval $(call external_library_source,https://github.com/wxWidgets/wxWidgets/releases/download/v$(WXWIDGETS_VERSION),wxWidgets-$(WXWIDGETS_VERSION)-headers.7z,40af582876d510f58124612ebfafd7826df986bca08e69f2f172b90f81e96762))
$(eval $(call external_library_source,https://github.com/wxWidgets/wxWidgets/releases/download/v$(WXWIDGETS_VERSION),wxMSW-$(WXWIDGETS_VERSION)_vc14x_Dev.7z,ca98ad6d290378b4ad96a0af7e2af4edb95449e8b75622d399f7e607c5429f11))
$(eval $(call external_library_source,https://github.com/wxWidgets/wxWidgets/releases/download/v$(WXWIDGETS_VERSION),wxMSW-$(WXWIDGETS_VERSION)_vc14x_x64_Dev.7z,5c8cfe8f3b90f2d854691237a243d8fa95fdc9644e305f3caca375ead6c6a115))
$(eval $(call external_library_source,https://github.com/wxWidgets/wxWidgets/releases/download/v$(WXWIDGETS_VERSION),wxMSW-$(WXWIDGETS_VERSION)_vc14x_ReleaseDLL.7z,ed15a362cefcbae9393cbfa1d9ff440d3b3e3aa541e7605f4f1892c4c75ce156))
$(eval $(call external_library_source,https://github.com/wxWidgets/wxWidgets/releases/download/v$(WXWIDGETS_VERSION),wxMSW-$(WXWIDGETS_VERSION)_vc14x_x64_ReleaseDLL.7z,7dc804f2df2ae45bcce710e2e51736843199dddf455f97106e9a23beb0d81d28))

WXWIDGETS_CODE = $(subst .,,$(WXWIDGETS_VERSION))
WXWIDGETS_DLL = \
	wxbase$(WXWIDGETS_CODE)u_$(MSVC_VARIANT).dll \
	wxbase$(WXWIDGETS_CODE)u_net_$(MSVC_VARIANT).dll \
	wxbase$(WXWIDGETS_CODE)u_xml_$(MSVC_VARIANT).dll \
	wxmsw$(WXWIDGETS_CODE)u_adv_$(MSVC_VARIANT).dll \
	wxmsw$(WXWIDGETS_CODE)u_aui_$(MSVC_VARIANT).dll \
	wxmsw$(WXWIDGETS_CODE)u_core_$(MSVC_VARIANT).dll \
	wxmsw$(WXWIDGETS_CODE)u_propgrid_$(MSVC_VARIANT).dll

define flint_add_wxWidgets_DLL_to_wxs
xmlstarlet ed -L \
		-s "//_:Component[@Id='wxmsw']" -t elem -n 'File' \
		--var file '$$prev' \
		-i '$$file' -t attr -n "Id" -v "$(subst -,_,$(subst .,_,$(notdir $(1))))" \
		-i '$$file' -t attr -n "Name" -v "$(1)" \
		-i '$$file' -t attr -n "DiskId" -v "1" \
		-i '$$file' -t attr -n "Source" -v "$(1)" \
		$(2)

endef

UNNECESSARY_LIBRARIES = libmicrohttpd-% wxWidgets-%
EXTERNAL_LIBRARIES = $(filter-out $(UNNECESSARY_LIBRARIES),$(ALL_EXTERNAL_LIBRARIES)) libmicrohttpd-$(LIBMICROHTTPD_VERSION)-w32-bin.zip wxWidgets-$(WXWIDGETS_VERSION)-headers.7z wxMSW-$(WXWIDGETS_VERSION)_$(MSVC_VARIANT)_Dev.7z wxMSW-$(WXWIDGETS_VERSION)_$(MSVC_VARIANT)_ReleaseDLL.7z sqlite-amalgamation-$(SQLITE_VSTR).zip sqlite-dll-win64-x64-$(SQLITE_VSTR).zip
EXTERNAL_LIBRARY_DIRS = $(filter-out $(UNNECESSARY_LIBRARIES),$(ALL_EXTERNAL_LIBRARY_DIRS)) libmicrohttpd-$(LIBMICROHTTPD_VERSION)-w32-bin include lib

flint_example_name = flint/example/$(1)

define flint_example
$(call flint_example_name,$(1)): ../example/$(1) | flint/example
	install -p -m 0644 $$< $$@
endef

define flint_add_example_to_wxs
xmlstarlet ed -L \
		-s "//_:Component[@Id='example']" -t elem -n 'File' \
		--var file '$$prev' \
		-i '$$file' -t attr -n "Id" -v "$(subst -,_,$(subst .,_,$(notdir $(1))))" \
		-i '$$file' -t attr -n "Name" -v "$(1)" \
		-i '$$file' -t attr -n "DiskId" -v "1" \
		-i '$$file' -t attr -n "Source" -v "example\\$(1)" \
		$(2)

endef

GENERATED_FILES = \
	all.proj \
	$(foreach lib,$(EXTERNAL_LIBRARY_DIRS),tmp/$(lib)) \
	tmp/release-dll \
	tmp/sqlite.header \
	tmp/sqlite.precompiled \
	flint/flint.ico \
	flint/flint.wxs \
	flint/license.rtf \
	flint/third-party-licenses.txt \
	$(foreach e,$(EXAMPLE_PHML) $(EXAMPLE_ISD),flint/example/$(e))

.PHONY: all mostlyclean clean distclean download

all: download $(GENERATED_FILES)

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

tmp/clibsedml-$(CLIBSEDML_VERSION): src/clibsedml-$(CLIBSEDML_VERSION).tar.gz clibsedml-$(CLIBSEDML_VERSION).patch | tmp
	-rm -rf var/clibsedml-$(CLIBSEDML_VERSION)
	tar -C var -zxf $<
	patch -d var/clibsedml-$(CLIBSEDML_VERSION) -p1 < clibsedml-$(CLIBSEDML_VERSION).patch
	touch $@

tmp/czmq-$(CZMQ_VERSION): src/czmq-$(CZMQ_VERSION).tar.gz | tmp
	-rm -rf var/czmq-$(CZMQ_VERSION)
	tar -C var -zxf $<
	touch $@

tmp/libsbml-$(LIBSBML_VERSION): src/libSBML-$(LIBSBML_VERSION)-core-src.zip | tmp
	-rm -rf var/libsbml-$(LIBSBML_VERSION)
	(cd var && unzip -qq ../$<)
	touch $@

tmp/libxml2-$(LIBXML2_VERSION): src/libxml2-$(LIBXML2_VERSION).tar.xz | tmp
	-rm -rf var/libxml2-$(LIBXML2_VERSION)
	tar -C var -xJf $<
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
	xmlstarlet ed -i '//_:Product' -t attr -n 'Version' -v $(VERSION) $< > $@.tmp
	$(foreach d,$(WXWIDGETS_DLL),$(call flint_add_wxWidgets_DLL_to_wxs,$(d),$@.tmp))
	$(foreach e,$(EXAMPLE_PHML) $(EXAMPLE_ISD),$(call flint_add_example_to_wxs,$(e),$@.tmp))
	mv $@.tmp $@

flint/license.rtf: ../doc/license.rtf | flint
	install -p -m 0644 $< $@

flint/third-party-licenses.txt: tmp/czmq-$(CZMQ_VERSION) tmp/libsbml-$(LIBSBML_VERSION) tmp/libxml2-$(LIBXML2_VERSION) tmp/protobuf-$(PROTOBUF_VERSION) tmp/SBML_odeSolver-$(SOSLIB_VERSION) tmp/sundials-$(SUNDIALS_VERSION) tmp/zeromq-$(ZEROMQ_VERSION) | flint
	echo ';;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;' > $@
	echo ';; Licenses of third-party libraries' >> $@
	echo ';;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;' >> $@
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

flint/example: | flint
	install -d $@

$(foreach e,$(EXAMPLE_PHML) $(EXAMPLE_ISD),$(eval $(call flint_example,$(e))))

all.proj: all.proj.in
	m4 \
		-D__TARGET_ARCH="$(TARGET_ARCH)" \
		-D__BOOST_UNDERSCORE="$(BOOST_UNDERSCORE)" \
		-D__B2_WITHOUT_OPTIONS="$(B2_WITHOUT_OPTIONS)" \
		-D__CLIBSEDML_VERSION="$(CLIBSEDML_VERSION)" \
		-D__CZMQ_VERSION="$(CZMQ_VERSION)" \
		-D__LIBMICROHTTPD_VERSION="$(LIBMICROHTTPD_VERSION)" \
		-D__LIBSBML_VERSION="$(LIBSBML_VERSION)" \
		-D__LIBXML2_VERSION="$(LIBXML2_VERSION)" \
		-D__PROTOBUF_VERSION="$(PROTOBUF_VERSION)" \
		-D__SOSLIB_VERSION="$(SOSLIB_VERSION)" \
		-D__SQLITE_VSTR="$(SQLITE_VSTR)" \
		-D__SUNDIALS_VERSION="$(SUNDIALS_VERSION)" \
		-D__WXWIDGETS_CODE="$(WXWIDGETS_CODE)" \
		-D__ZEROMQ_VERSION="$(ZEROMQ_VERSION)" \
		$< > $@

.DEFAULT_GOAL := all
