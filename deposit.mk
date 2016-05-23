DEPOSIT_URL = http://www.physiodesigner.org/developer/sources

CLIBSEDML_COMMIT = d8596b56
SOSLIB_COMMIT = 87bfbc8d

BOOST_VERSION = 1.61.0
LIBSBML_VERSION = 5.11.4
LIBXML2_VERSION = 2.7.8
LIBZIP_VERSION = 1.1.1
PROTOBUF_VERSION = 2.6.1
SUNDIALS_VERSION = 2.6.2
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

$(eval $(call external_library_source,$(BOOST_UNDERSCORE).tar.bz2,6095876341956f65f9d35939ccea1a9f))
$(eval $(call external_library_source,clibsedml-$(CLIBSEDML_COMMIT).tar.gz,d780b4f02d5272e3c0b2d4610e6d6c3f))
$(eval $(call external_library_source,libSBML-$(LIBSBML_VERSION)-core-src.zip,d854f58ba65c685692bff6031b78db27))
$(eval $(call external_library_source,libxml2-sources-$(LIBXML2_VERSION).tar.gz,a78857dd73a8784776d7f9625ccf7a39))
$(eval $(call external_library_source,libzip-$(LIBZIP_VERSION).tar.gz,133aefc4c7e45a1b7d168a617e289ef6))
$(eval $(call external_library_source,protobuf-$(PROTOBUF_VERSION).tar.gz,f3916ce13b7fcb3072a1fa8cf02b2423))
$(eval $(call external_library_source,SBML_odeSolver-$(SOSLIB_COMMIT).tar.gz,c44cf16d5c7aab2176faeafa8cacb62d))
$(eval $(call external_library_source,sundials-$(SUNDIALS_VERSION).tar.gz,3deeb0ede9f514184c6bd83ecab77d95))
$(eval $(call external_library_source,zlib-$(ZLIB_VERSION).tar.gz,44d667c142d7cda120332623eab69f40))
