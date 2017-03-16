BOOST_VERSION = 1.63.0
CLIBSEDML_VERSION = 0.1.0
CZMQ_VERSION = 4.0.2
LIBSBML_VERSION = 5.13.0
LIBXML2_VERSION = 2.9.4
PROTOBUF_VERSION = 3.2.0
SOSLIB_VERSION = 1.9.0
SUNDIALS_VERSION = 2.7.0
WXWIDGETS_VERSION = 3.1.0
ZEROMQ_VERSION = 4.2.1

CZMQ_CURRENT = 4
PROTOBUF_CURRENT = 12
ZEROMQ_CURRENT = 5

CZMQ_VERSION_INFO = $(CZMQ_CURRENT).0.2
ZEROMQ_VERSION_INFO = $(ZEROMQ_CURRENT).1.1

BOOST_UNDERSCORE = boost_$(subst .,_,$(BOOST_VERSION))

ALL_EXTERNAL_LIBRARIES = \
	$(BOOST_UNDERSCORE).tar.bz2 \
	clibsedml-$(CLIBSEDML_VERSION).tar.gz \
	czmq-$(CZMQ_VERSION).tar.gz \
	libSBML-$(LIBSBML_VERSION)-core-src.zip \
	libxml2-$(LIBXML2_VERSION).tar.gz \
	protobuf-java-$(PROTOBUF_VERSION).tar.gz \
	SBML_odeSolver-$(SOSLIB_VERSION).tar.gz \
	sundials-$(SUNDIALS_VERSION).tar.gz \
	wxWidgets-$(WXWIDGETS_VERSION).tar.bz2 \
	zeromq-$(ZEROMQ_VERSION).tar.gz

ALL_EXTERNAL_LIBRARY_DIRS = \
	$(BOOST_UNDERSCORE) \
	clibsedml-$(CLIBSEDML_VERSION) \
	czmq-$(CZMQ_VERSION) \
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
$(eval $(call external_library_source3,https://github.com/flintproject/clibsedml/archive/v$(CLIBSEDML_VERSION).tar.gz,clibsedml-$(CLIBSEDML_VERSION).tar.gz,72a4c7ddfd2cdf5c3716c08b422dc7d8))
$(eval $(call external_library_source,https://github.com/zeromq/czmq/releases/download/v$(CZMQ_VERSION),czmq-$(CZMQ_VERSION).tar.gz,b27cb5a23c472949b1e37765e404dc98))
$(eval $(call external_library_source,http://downloads.sourceforge.net/project/sbml/libsbml/$(LIBSBML_VERSION)/stable,libSBML-$(LIBSBML_VERSION)-core-src.zip,6581723e894eee8058b95fa80df7aad4))
$(eval $(call external_library_source,ftp://xmlsoft.org/libxml2,libxml2-$(LIBXML2_VERSION).tar.gz,ae249165c173b1ff386ee8ad676815f5))
$(eval $(call external_library_source,https://github.com/google/protobuf/releases/download/v$(PROTOBUF_VERSION),protobuf-java-$(PROTOBUF_VERSION).tar.gz,74da013866efbb8850ceca981917c63a))
$(eval $(call external_library_source3,https://github.com/raim/SBML_odeSolver/archive/$(SOSLIB_VERSION).tar.gz,SBML_odeSolver-$(SOSLIB_VERSION).tar.gz,a2223179576e33eff110065d4481e306))
$(eval $(call external_library_source,http://pkgs.fedoraproject.org/repo/extras/sundials/sundials-$(SUNDIALS_VERSION).tar.gz/c304631b9bc82877d7b0e9f4d4fd94d3,sundials-$(SUNDIALS_VERSION).tar.gz,c304631b9bc82877d7b0e9f4d4fd94d3))
$(eval $(call external_library_source,https://github.com/wxWidgets/wxWidgets/releases/download/v$(WXWIDGETS_VERSION),wxWidgets-$(WXWIDGETS_VERSION).tar.bz2,e20c14bb9bf5d4ec0979a3cd7510dece))
$(eval $(call external_library_source,https://github.com/zeromq/libzmq/releases/download/v$(ZEROMQ_VERSION),zeromq-$(ZEROMQ_VERSION).tar.gz,820cec2860a72c3257881a394d83bfc0))
