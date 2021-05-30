# -*- Mode: makefile-gmake; tab-width: 4; indent-tabs-mode: t -*-
BOOST_VERSION = 1.75.0
CERES_VERSION = 1.14.0
CLIBSEDML_VERSION = 0.1.2
CZMQ_VERSION = 4.2.1
LIBMICROHTTPD_VERSION = 0.9.73
LIBSBML_VERSION = 5.17.0
LIBXML2_VERSION = 2.9.12
PROTOBUF_VERSION = 3.14.0
SOSLIB_VERSION = 1.9.0
SUNDIALS_VERSION = 2.7.0
WXWIDGETS_VERSION = 3.1.4
ZEROMQ_VERSION = 4.3.4

CERES_CURRENT = 1
CZMQ_CURRENT = 4
LIBMICROHTTPD_CURRENT = 12
PROTOBUF_CURRENT_MAJOR = 25
PROTOBUF_CURRENT_TINY = 0
ZEROMQ_CURRENT = 5

CZMQ_VERSION_INFO = $(CZMQ_CURRENT).2.1
ZEROMQ_VERSION_INFO = $(ZEROMQ_CURRENT).2.4

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

$(eval $(call external_library_source,https://boostorg.jfrog.io/artifactory/main/release/$(BOOST_VERSION)/source,$(BOOST_UNDERSCORE).tar.bz2,953db31e016db7bb207f11432bef7df100516eeb746843fa0486a222e3fd49cb))
$(eval $(call external_library_source,http://ceres-solver.org/,ceres-solver-$(CERES_VERSION).tar.gz,4744005fc3b902fed886ea418df70690caa8e2ff6b5a90f3dd88a3d291ef8e8e))
$(eval $(call external_library_source3,https://github.com/flintproject/clibsedml/archive/v$(CLIBSEDML_VERSION).tar.gz,clibsedml-$(CLIBSEDML_VERSION).tar.gz,cf25d0aad379e14c26a74166b5d652e2072fc87183caf6f34c16b5122ffa469f))
$(eval $(call external_library_source,https://github.com/zeromq/czmq/releases/download/v$(CZMQ_VERSION),czmq-$(CZMQ_VERSION).tar.gz,5d720a204c2a58645d6f7643af15d563a712dad98c9d32c1ed913377daa6ac39))
$(eval $(call external_library_source,https://ftp.gnu.org/gnu/libmicrohttpd,libmicrohttpd-$(LIBMICROHTTPD_VERSION).tar.gz,a37b2f1b88fd1bfe74109586be463a434d34e773530fc2a74364cfcf734c032e))
$(eval $(call external_library_source,http://downloads.sourceforge.net/project/sbml/libsbml/$(LIBSBML_VERSION)/stable,libSBML-$(LIBSBML_VERSION)-core-src.zip,76d6c1e9bbe966204db602d9595b6536fc96ff6af7404d2ca1df3225a0a721b4))
$(eval $(call external_library_source,ftp://xmlsoft.org/libxml2,libxml2-$(LIBXML2_VERSION).tar.gz,8a4ddd706419c210b30b8978a51388937fd9362c34fc9a3d69e4fcc6f8055ee0))
$(eval $(call external_library_source,https://github.com/google/protobuf/releases/download/v$(PROTOBUF_VERSION),protobuf-cpp-$(PROTOBUF_VERSION).tar.gz,50ec5a07c0c55d4ec536dd49021f2e194a26bfdbc531d03d1e9d4d3e27175659))
$(eval $(call external_library_source3,https://github.com/raim/SBML_odeSolver/archive/$(SOSLIB_VERSION).tar.gz,SBML_odeSolver-$(SOSLIB_VERSION).tar.gz,059c2f51f52e1ac29d3873a2d3fcc71947e1a196f29816067041c976cb5cc651))
$(eval $(call external_library_source,https://github.com/LLNL/sundials/releases/download/v$(SUNDIALS_VERSION),sundials-$(SUNDIALS_VERSION).tar.gz,d39fcac7175d701398e4eb209f7e92a5b30a78358d4a0c0fcc23db23c11ba104))
$(eval $(call external_library_source,https://github.com/wxWidgets/wxWidgets/releases/download/v$(WXWIDGETS_VERSION),wxWidgets-$(WXWIDGETS_VERSION).tar.bz2,3ca3a19a14b407d0cdda507a7930c2e84ae1c8e74f946e0144d2fa7d881f1a94))
$(eval $(call external_library_source,https://github.com/zeromq/libzmq/releases/download/v$(ZEROMQ_VERSION),zeromq-$(ZEROMQ_VERSION).tar.gz,c593001a89f5a85dd2ddf564805deb860e02471171b3f204944857336295c3e5))
