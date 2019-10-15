# -*- Mode: makefile-gmake; tab-width: 4; indent-tabs-mode: t -*-
BOOST_VERSION = 1.70.0
CERES_VERSION = 1.14.0
CLIBSEDML_VERSION = 0.1.2
CZMQ_VERSION = 4.2.0
LIBMICROHTTPD_VERSION = 0.9.64
LIBSBML_VERSION = 5.17.0
LIBXML2_VERSION = 2.9.7
PROTOBUF_VERSION = 3.10.0
SOSLIB_VERSION = 1.9.0
SUNDIALS_VERSION = 2.7.0
WXWIDGETS_VERSION = 3.1.2
ZEROMQ_VERSION = 4.3.2

CERES_CURRENT = 1
CZMQ_CURRENT = 4
LIBMICROHTTPD_CURRENT = 12
PROTOBUF_CURRENT = 21
ZEROMQ_CURRENT = 5

CZMQ_VERSION_INFO = $(CZMQ_CURRENT).2.0
ZEROMQ_VERSION_INFO = $(ZEROMQ_CURRENT).2.2

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

$(eval $(call external_library_source,https://dl.bintray.com/boostorg/release/$(BOOST_VERSION)/source,$(BOOST_UNDERSCORE).tar.bz2,430ae8354789de4fd19ee52f3b1f739e1fba576f0aded0897c3c2bc00fb38778))
$(eval $(call external_library_source,http://ceres-solver.org/,ceres-solver-$(CERES_VERSION).tar.gz,4744005fc3b902fed886ea418df70690caa8e2ff6b5a90f3dd88a3d291ef8e8e))
$(eval $(call external_library_source3,https://github.com/flintproject/clibsedml/archive/v$(CLIBSEDML_VERSION).tar.gz,clibsedml-$(CLIBSEDML_VERSION).tar.gz,cf25d0aad379e14c26a74166b5d652e2072fc87183caf6f34c16b5122ffa469f))
$(eval $(call external_library_source,https://github.com/zeromq/czmq/releases/download/v$(CZMQ_VERSION),czmq-$(CZMQ_VERSION).tar.gz,cfab29c2b3cc8a845749758a51e1dd5f5160c1ef57e2a41ea96e4c2dcc8feceb))
$(eval $(call external_library_source,https://ftp.gnu.org/gnu/libmicrohttpd,libmicrohttpd-$(LIBMICROHTTPD_VERSION).tar.gz,e792d8ed5990823a0baadea0adf94365999e702f6f1314ef9c555018dafc350e))
$(eval $(call external_library_source,http://downloads.sourceforge.net/project/sbml/libsbml/$(LIBSBML_VERSION)/stable,libSBML-$(LIBSBML_VERSION)-core-src.zip,76d6c1e9bbe966204db602d9595b6536fc96ff6af7404d2ca1df3225a0a721b4))
$(eval $(call external_library_source,ftp://xmlsoft.org/libxml2,libxml2-$(LIBXML2_VERSION).tar.gz,f63c5e7d30362ed28b38bfa1ac6313f9a80230720b7fb6c80575eeab3ff5900c))
$(eval $(call external_library_source,https://github.com/google/protobuf/releases/download/v$(PROTOBUF_VERSION),protobuf-cpp-$(PROTOBUF_VERSION).tar.gz,ffb91e102e8c389fba6fefec948421852979ae655ebb52e69859a6a3f4c5b61b))
$(eval $(call external_library_source3,https://github.com/raim/SBML_odeSolver/archive/$(SOSLIB_VERSION).tar.gz,SBML_odeSolver-$(SOSLIB_VERSION).tar.gz,059c2f51f52e1ac29d3873a2d3fcc71947e1a196f29816067041c976cb5cc651))
$(eval $(call external_library_source,https://github.com/LLNL/sundials/releases/download/v$(SUNDIALS_VERSION),sundials-$(SUNDIALS_VERSION).tar.gz,d39fcac7175d701398e4eb209f7e92a5b30a78358d4a0c0fcc23db23c11ba104))
$(eval $(call external_library_source,https://github.com/wxWidgets/wxWidgets/releases/download/v$(WXWIDGETS_VERSION),wxWidgets-$(WXWIDGETS_VERSION).tar.bz2,4cb8d23d70f9261debf7d6cfeca667fc0a7d2b6565adb8f1c484f9b674f1f27a))
$(eval $(call external_library_source,https://github.com/zeromq/libzmq/releases/download/v$(ZEROMQ_VERSION),zeromq-$(ZEROMQ_VERSION).tar.gz,ebd7b5c830d6428956b67a0454a7f8cbed1de74b3b01e5c33c5378e22740f763))
