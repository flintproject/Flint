DEPOSIT_URL = http://www.physiodesigner.org/developer/sources

CLIBSEDML_COMMIT = d8596b56

BOOST_VERSION = 1.63.0
LIBSBML_VERSION = 5.13.0
LIBXML2_VERSION = 2.7.8
LIBZIP_VERSION = 1.1.1
PROTOBUF_VERSION = 3.2.0
SOSLIB_VERSION = 1.9.0
SUNDIALS_VERSION = 2.7.0
WXWIDGETS_VERSION = 3.1.0
ZLIB_VERSION = 1.2.11

BOOST_UNDERSCORE = boost_$(subst .,_,$(BOOST_VERSION))

EXTERNAL_LIBRARIES = \
	$(BOOST_UNDERSCORE).tar.bz2 \
	clibsedml-$(CLIBSEDML_COMMIT).tar.gz \
	libSBML-$(LIBSBML_VERSION)-core-src.zip \
	libxml2-sources-$(LIBXML2_VERSION).tar.gz \
	libzip-$(LIBZIP_VERSION).tar.gz \
	protobuf-java-$(PROTOBUF_VERSION).tar.gz \
	SBML_odeSolver-$(SOSLIB_VERSION).tar.gz \
	sundials-$(SUNDIALS_VERSION).tar.gz \
	wxWidgets-$(WXWIDGETS_VERSION).tar.bz2 \
	zlib-$(ZLIB_VERSION).tar.gz

EXTERNAL_LIBRARY_DIRS = \
	$(BOOST_UNDERSCORE) \
	clibsedml-$(CLIBSEDML_COMMIT) \
	libsbml-$(LIBSBML_VERSION) \
	libxml2-$(LIBXML2_VERSION) \
	libzip-$(LIBZIP_VERSION) \
	protobuf-$(PROTOBUF_VERSION) \
	SBML_odeSolver-$(SOSLIB_VERSION) \
	sundials-$(SUNDIALS_VERSION) \
	wxWidgets-$(WXWIDGETS_VERSION) \
	zlib-$(ZLIB_VERSION)

OS := $(shell uname -s)

src:
	install -d $@

ifeq ($(OS),Darwin)

define external_library_source
src/$(2): | src
	curl -L -o $$@ $(1)/$(2)
	md5 $$@ | grep $(3)
endef

define external_library_source3
src/$(2): | src
	curl -L -o $$@ $(1)
	md5 $$@ | grep $(3)
endef

else ifeq ($(OS),FreeBSD)

define external_library_source
src/$(2): | src
	curl -L -o $$@ $(1)/$(2)
	md5 $$@ | grep $(3)
endef

define external_library_source3
src/$(2): | src
	curl -L -o $$@ $(1)
	md5 $$@ | grep $(3)
endef

else

define external_library_source
src/$(2): | src
	wget -O $$@ $(1)/$(2)
	test "$(3)" = `md5sum $$@ | cut -d' ' -f1`
endef

define external_library_source3
src/$(2): | src
	wget -O $$@ $(1)
	test "$(3)" = `md5sum $$@ | cut -d' ' -f1`
endef

endif

$(eval $(call external_library_source,http://downloads.sourceforge.net/project/boost/boost/$(BOOST_VERSION),$(BOOST_UNDERSCORE).tar.bz2,1c837ecd990bb022d07e7aab32b09847))
$(eval $(call external_library_source,$(DEPOSIT_URL),clibsedml-$(CLIBSEDML_COMMIT).tar.gz,d780b4f02d5272e3c0b2d4610e6d6c3f))
$(eval $(call external_library_source,http://downloads.sourceforge.net/project/sbml/libsbml/$(LIBSBML_VERSION)/stable,libSBML-$(LIBSBML_VERSION)-core-src.zip,6581723e894eee8058b95fa80df7aad4))
$(eval $(call external_library_source,http://xmlsoft.org/sources,libxml2-sources-$(LIBXML2_VERSION).tar.gz,a78857dd73a8784776d7f9625ccf7a39))
$(eval $(call external_library_source,http://www.nih.at/libzip,libzip-$(LIBZIP_VERSION).tar.gz,133aefc4c7e45a1b7d168a617e289ef6))
$(eval $(call external_library_source,https://github.com/google/protobuf/releases/download/v$(PROTOBUF_VERSION),protobuf-java-$(PROTOBUF_VERSION).tar.gz,74da013866efbb8850ceca981917c63a))
$(eval $(call external_library_source3,https://github.com/raim/SBML_odeSolver/archive/$(SOSLIB_VERSION).tar.gz,SBML_odeSolver-$(SOSLIB_VERSION).tar.gz,a2223179576e33eff110065d4481e306))
$(eval $(call external_library_source,http://pkgs.fedoraproject.org/repo/extras/sundials/sundials-$(SUNDIALS_VERSION).tar.gz/c304631b9bc82877d7b0e9f4d4fd94d3,sundials-$(SUNDIALS_VERSION).tar.gz,c304631b9bc82877d7b0e9f4d4fd94d3))
$(eval $(call external_library_source,https://github.com/wxWidgets/wxWidgets/releases/download/v$(WXWIDGETS_VERSION),wxWidgets-$(WXWIDGETS_VERSION).tar.bz2,e20c14bb9bf5d4ec0979a3cd7510dece))
$(eval $(call external_library_source,http://zlib.net,zlib-$(ZLIB_VERSION).tar.gz,1c9f62f0778697a09d36121ead88e08e))
