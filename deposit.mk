DEPOSIT_URL = http://www.physiodesigner.org/developer/sources

CLIBSEDML_COMMIT = d8596b56
SOSLIB_COMMIT = 87bfbc8d

BOOST_VERSION = 1.61.0
LIBSBML_VERSION = 5.13.0
LIBXML2_VERSION = 2.7.8
LIBZIP_VERSION = 1.1.1
PROTOBUF_VERSION = 3.0.0
SUNDIALS_VERSION = 2.6.2
ZLIB_VERSION = 1.2.8

BOOST_UNDERSCORE = boost_$(subst .,_,$(BOOST_VERSION))

EXTERNAL_LIBRARIES = \
	$(BOOST_UNDERSCORE).tar.bz2 \
	clibsedml-$(CLIBSEDML_COMMIT).tar.gz \
	libSBML-$(LIBSBML_VERSION)-core-src.zip \
	libxml2-sources-$(LIBXML2_VERSION).tar.gz \
	libzip-$(LIBZIP_VERSION).tar.gz \
	protobuf-java-$(PROTOBUF_VERSION).tar.gz \
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
src/$(2): | src
	curl -L -o $$@ $(1)/$(2)
	md5 $$@ | grep $(3)
endef

else ifeq ($(OS),FreeBSD)

define external_library_source
src/$(2): | src
	curl -L -o $$@ $(1)/$(2)
	md5 $$@ | grep $(3)
endef

else

define external_library_source
src/$(2): | src
	wget -O $$@ $(1)/$(2)
	test "$(3)" = `md5sum $$@ | cut -d' ' -f1`
endef

endif

$(eval $(call external_library_source,http://downloads.sourceforge.net/project/boost/boost/$(BOOST_VERSION),$(BOOST_UNDERSCORE).tar.bz2,6095876341956f65f9d35939ccea1a9f))
$(eval $(call external_library_source,$(DEPOSIT_URL),clibsedml-$(CLIBSEDML_COMMIT).tar.gz,d780b4f02d5272e3c0b2d4610e6d6c3f))
$(eval $(call external_library_source,http://downloads.sourceforge.net/project/sbml/libsbml/$(LIBSBML_VERSION)/stable,libSBML-$(LIBSBML_VERSION)-core-src.zip,6581723e894eee8058b95fa80df7aad4))
$(eval $(call external_library_source,http://xmlsoft.org/sources,libxml2-sources-$(LIBXML2_VERSION).tar.gz,a78857dd73a8784776d7f9625ccf7a39))
$(eval $(call external_library_source,http://www.nih.at/libzip,libzip-$(LIBZIP_VERSION).tar.gz,133aefc4c7e45a1b7d168a617e289ef6))
$(eval $(call external_library_source,https://github.com/google/protobuf/releases/download/v$(PROTOBUF_VERSION),protobuf-java-$(PROTOBUF_VERSION).tar.gz,c5002fc32110450d06252ad6c53bb879))
$(eval $(call external_library_source,$(DEPOSIT_URL),SBML_odeSolver-$(SOSLIB_COMMIT).tar.gz,c44cf16d5c7aab2176faeafa8cacb62d))
$(eval $(call external_library_source,http://pkgs.fedoraproject.org/repo/extras/sundials/sundials-$(SUNDIALS_VERSION).tar.gz/3deeb0ede9f514184c6bd83ecab77d95,sundials-$(SUNDIALS_VERSION).tar.gz,3deeb0ede9f514184c6bd83ecab77d95))
$(eval $(call external_library_source,http://zlib.net,zlib-$(ZLIB_VERSION).tar.gz,44d667c142d7cda120332623eab69f40))
