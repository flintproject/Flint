DEPOSIT_URL = http://www.physiodesigner.org/developer/sources

CLIBSEDML_COMMIT = d8596b56
SOSLIB_COMMIT = cc087c96

BOOST_VERSION = 1.59.0
LIBSBML_VERSION = 5.11.4
LIBXML2_VERSION = 2.7.8
LIBZIP_VERSION = 0.11.2
PROTOBUF_VERSION = 2.6.1
SUNDIALS_VERSION = 2.3.0
ZLIB_VERSION = 1.2.8

BOOST_UNDERSCORE = boost_$(subst .,_,$(BOOST_VERSION))

EXTERNAL_LIBRARIES = \
	$(BOOST_UNDERSCORE).tar.bz2 \
	clibsedml-$(CLIBSEDML_COMMIT).tar.gz \
	libSBML-$(LIBSBML_VERSION)-core-src.zip \
	libxml2-sources-$(LIBXML2_VERSION).tar.gz \
	libzip-$(LIBZIP_VERSION).tar.gz \
	protobuf-$(PROTOBUF_VERSION).tar.gz \
	SBML_odeSolver-$(SOSLIB_COMMIT).tar.gz \
	sundials-$(SUNDIALS_VERSION).tar.gz \
	zlib-$(ZLIB_VERSION).tar.gz

EXTERNAL_LIBRARY_DIRS = \
	$(BOOST_UNDERSCORE) \
	clibsedml-$(CLIBSEDML_COMMIT) \
	libsbml-$(LIBSBML_VERSION) \
	libxml2-$(LIBXML2_VERSION) \
	libzip-$(LIBZIP_VERSION) \
	protobuf-$(PROTOBUF_VERSION) \
	SBML_odeSolver-$(SOSLIB_COMMIT) \
	sundials-$(SUNDIALS_VERSION) \
	zlib-$(ZLIB_VERSION)

OS := $(shell uname -s)

src:
	install -d $@

ifeq ($(OS),Darwin)

define external_library_source
src/$(1): | src
	curl $(DEPOSIT_URL)/$(1) -o $$@
	md5 $$@ | grep $(2)
endef

else ifeq ($(OS),FreeBSD)

define external_library_source
src/$(1): | src
	curl $(DEPOSIT_URL)/$(1) -o $$@
	md5 $$@ | grep $(2)
endef

else

define external_library_source
src/$(1): | src
	wget -O $$@ $(DEPOSIT_URL)/$(1)
	test "$(2)" = `md5sum $$@ | cut -d' ' -f1`
endef

endif

$(eval $(call external_library_source,$(BOOST_UNDERSCORE).tar.bz2,6aa9a5c6a4ca1016edd0ed1178e3cb87))
$(eval $(call external_library_source,clibsedml-$(CLIBSEDML_COMMIT).tar.gz,d780b4f02d5272e3c0b2d4610e6d6c3f))
$(eval $(call external_library_source,libSBML-$(LIBSBML_VERSION)-core-src.zip,d854f58ba65c685692bff6031b78db27))
$(eval $(call external_library_source,libxml2-sources-$(LIBXML2_VERSION).tar.gz,a78857dd73a8784776d7f9625ccf7a39))
$(eval $(call external_library_source,libzip-$(LIBZIP_VERSION).tar.gz,c5437df15e4825d40cdc3ec8b9b7516c))
$(eval $(call external_library_source,protobuf-$(PROTOBUF_VERSION).tar.gz,f3916ce13b7fcb3072a1fa8cf02b2423))
$(eval $(call external_library_source,SBML_odeSolver-$(SOSLIB_COMMIT).tar.gz,2c391aff53b0cadfdd59d36ec629071a))
$(eval $(call external_library_source,sundials-$(SUNDIALS_VERSION).tar.gz,c236f2a7e0e6a03b8fab3d189471b933))
$(eval $(call external_library_source,zlib-$(ZLIB_VERSION).tar.gz,44d667c142d7cda120332623eab69f40))
