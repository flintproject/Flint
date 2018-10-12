BOOST_VERSION = 1.68.0
CERES_VERSION = 1.14.0
CLIBSEDML_VERSION = 0.1.2
CZMQ_VERSION = 4.1.1
LIBMICROHTTPD_VERSION = 0.9.58
LIBSBML_VERSION = 5.17.0
LIBXML2_VERSION = 2.9.7
PROTOBUF_VERSION = 3.6.1
SOSLIB_VERSION = 1.9.0
SUNDIALS_VERSION = 2.7.0
WXWIDGETS_VERSION = 3.1.1
ZEROMQ_VERSION = 4.2.4

CERES_CURRENT = 1
CZMQ_CURRENT = 4
LIBMICROHTTPD_CURRENT = 12
PROTOBUF_CURRENT = 17
ZEROMQ_CURRENT = 5

CZMQ_VERSION_INFO = $(CZMQ_CURRENT).1.1
ZEROMQ_VERSION_INFO = $(ZEROMQ_CURRENT).1.4

BOOST_UNDERSCORE = boost_$(subst .,_,$(BOOST_VERSION))

ALL_EXTERNAL_LIBRARIES = \
	$(BOOST_UNDERSCORE).tar.bz2 \
	ceres-solver-$(CERES_VERSION).tar.gz \
	clibsedml-$(CLIBSEDML_VERSION).tar.gz \
	czmq-$(CZMQ_VERSION).tar.gz \
	libmicrohttpd-$(LIBMICROHTTPD_VERSION).tar.gz \
	libSBML-$(LIBSBML_VERSION)-core-src.zip \
	libxml2-$(LIBXML2_VERSION).tar.gz \
	protobuf-cpp-$(PROTOBUF_VERSION).tar.gz \
	SBML_odeSolver-$(SOSLIB_VERSION).tar.gz \
	sundials-$(SUNDIALS_VERSION).tar.gz \
	wxWidgets-$(WXWIDGETS_VERSION).tar.bz2 \
	zeromq-$(ZEROMQ_VERSION).tar.gz

ALL_EXTERNAL_LIBRARY_DIRS = \
	$(BOOST_UNDERSCORE) \
	ceres-solver-$(CERES_VERSION) \
	clibsedml-$(CLIBSEDML_VERSION) \
	czmq-$(CZMQ_VERSION) \
	libmicrohttpd-$(LIBMICROHTTPD_VERSION) \
	libsbml-$(LIBSBML_VERSION) \
	libxml2-$(LIBXML2_VERSION) \
	protobuf-$(PROTOBUF_VERSION) \
	SBML_odeSolver-$(SOSLIB_VERSION) \
	sundials-$(SUNDIALS_VERSION) \
	wxWidgets-$(WXWIDGETS_VERSION) \
	zeromq-$(ZEROMQ_VERSION)

OS := $(shell uname -s)

src:
	install -d $@

ifeq ($(OS),Darwin)

define external_library_source
src/$(2): | src
	curl -L -o $$@ $(1)/$(2)
	shasum -a 256 $$@ | grep $(3)
endef

define external_library_source3
src/$(2): | src
	curl -L -o $$@ $(1)
	shasum -a 256 $$@ | grep $(3)
endef

else ifeq ($(OS),FreeBSD)

define external_library_source
src/$(2): | src
	curl -L -o $$@ $(1)/$(2)
	shasum -a 256 $$@ | grep $(3)
endef

define external_library_source3
src/$(2): | src
	curl -L -o $$@ $(1)
	shasum -a 256 $$@ | grep $(3)
endef

else

define external_library_source
src/$(2): | src
	wget -O $$@ $(1)/$(2)
	test "$(3)" = `sha256sum $$@ | cut -d' ' -f1`
endef

define external_library_source3
src/$(2): | src
	wget -O $$@ $(1)
	test "$(3)" = `sha256sum $$@ | cut -d' ' -f1`
endef

endif

$(eval $(call external_library_source,https://dl.bintray.com/boostorg/release/$(BOOST_VERSION)/source,$(BOOST_UNDERSCORE).tar.bz2,7f6130bc3cf65f56a618888ce9d5ea704fa10b462be126ad053e80e553d6d8b7))
$(eval $(call external_library_source,http://ceres-solver.org/,ceres-solver-$(CERES_VERSION).tar.gz,4744005fc3b902fed886ea418df70690caa8e2ff6b5a90f3dd88a3d291ef8e8e))
$(eval $(call external_library_source3,https://github.com/flintproject/clibsedml/archive/v$(CLIBSEDML_VERSION).tar.gz,clibsedml-$(CLIBSEDML_VERSION).tar.gz,cf25d0aad379e14c26a74166b5d652e2072fc87183caf6f34c16b5122ffa469f))
$(eval $(call external_library_source,https://github.com/zeromq/czmq/releases/download/v$(CZMQ_VERSION),czmq-$(CZMQ_VERSION).tar.gz,f00ff419881dc2a05d0686c8467cd89b4882677fc56f31c0e2cc81c134cbb0c0))
$(eval $(call external_library_source,https://ftp.gnu.org/gnu/libmicrohttpd,libmicrohttpd-$(LIBMICROHTTPD_VERSION).tar.gz,7a11e1376c62ff95bd6d2dfe6799d57ac7cdbcb32f70bfbd5e47c71f373e01f3))
$(eval $(call external_library_source,http://downloads.sourceforge.net/project/sbml/libsbml/$(LIBSBML_VERSION)/stable,libSBML-$(LIBSBML_VERSION)-core-src.zip,76d6c1e9bbe966204db602d9595b6536fc96ff6af7404d2ca1df3225a0a721b4))
$(eval $(call external_library_source,ftp://xmlsoft.org/libxml2,libxml2-$(LIBXML2_VERSION).tar.gz,f63c5e7d30362ed28b38bfa1ac6313f9a80230720b7fb6c80575eeab3ff5900c))
$(eval $(call external_library_source,https://github.com/google/protobuf/releases/download/v$(PROTOBUF_VERSION),protobuf-cpp-$(PROTOBUF_VERSION).tar.gz,b3732e471a9bb7950f090fd0457ebd2536a9ba0891b7f3785919c654fe2a2529))
$(eval $(call external_library_source3,https://github.com/raim/SBML_odeSolver/archive/$(SOSLIB_VERSION).tar.gz,SBML_odeSolver-$(SOSLIB_VERSION).tar.gz,059c2f51f52e1ac29d3873a2d3fcc71947e1a196f29816067041c976cb5cc651))
$(eval $(call external_library_source,https://github.com/LLNL/sundials/releases/download/v$(SUNDIALS_VERSION),sundials-$(SUNDIALS_VERSION).tar.gz,d39fcac7175d701398e4eb209f7e92a5b30a78358d4a0c0fcc23db23c11ba104))
$(eval $(call external_library_source,https://github.com/wxWidgets/wxWidgets/releases/download/v$(WXWIDGETS_VERSION),wxWidgets-$(WXWIDGETS_VERSION).tar.bz2,c925dfe17e8f8b09eb7ea9bfdcfcc13696a3e14e92750effd839f5e10726159e))
$(eval $(call external_library_source,https://github.com/zeromq/libzmq/releases/download/v$(ZEROMQ_VERSION),zeromq-$(ZEROMQ_VERSION).tar.gz,1ad5c28d44e31f0f7cad09b95bafaca8cc5346ff24a63a5e7d01c954162ad536))
