BOOST_UNUSED_COMPONENTS = \
	atomic \
	chrono \
	container \
	context \
	coroutine \
	date_time \
	exception \
	fiber \
	graph \
	graph_parallel \
	iostreams \
	locale \
	log \
	metaparse \
	mpi \
	python \
	regex \
	stacktrace \
	serialization \
	signals \
	thread \
	timer \
	type_erasure \
	wave

B2_WITHOUT_OPTIONS = $(foreach c,$(BOOST_UNUSED_COMPONENTS),--without-$(c))

CERES_CMAKE_OPTIONS = -DMINIGLOG=ON -DBUILD_SHARED_LIBS=ON

CLIBSEDML_CONFIGURE_OPTIONS = --enable-silent-rules --disable-static

CZMQ_CONFIGURE_OPTIONS = \
	--enable-silent-rules \
	--disable-static \
	--disable-Werror

LIBMICROHTTPD_CONFIGURE_OPTIONS = \
	--enable-silent-rules \
	--disable-static \
	--disable-curl \
	--disable-https \
	--disable-epoll

SOSLIB_CONFIGURE_OPTIONS = \
	--disable-static \
	--without-grace \
	--without-graphviz \
	--without-xerces

LIBSBML_CONFIGURE_OPTIONS = \
	--disable-static \
	--without-bzip2 \
	--without-expat \
	--without-xerces \
	--without-zlib

LIBXML2_CONFIGURE_OPTIONS = \
	--disable-static \
	--without-debug \
	--without-iconv \
	--without-icu \
	--without-ftp \
	--without-http \
	--without-lzma \
	--without-python \
	--without-threads \
	--without-zlib

PROTOBUF_CONFIGURE_OPTONS = \
	--enable-silent-rules \
	--disable-static \
	--without-zlib

SUNDIALS_CMAKE_OPTIONS = \
	-DBUILD_STATIC_LIBS=OFF \
	-DBUILD_SHARED_LIBS=ON \
	-DEXAMPLES_ENABLE=OFF

WXWIDGETS_CONFIGURE_OPTIONS = \
	--enable-arcstream \
	--enable-aui \
	--enable-cxx11 \
	--enable-dnd \
	--enable-monolithic \
	--enable-propgrid \
	--enable-streams \
	--enable-timer \
	--enable-zipstream \
	--with-zlib \
	--disable-compat30 \
	--disable-constraints \
	--disable-ftp \
	--disable-help \
	--disable-html \
	--disable-htmlhelp \
	--disable-mediactrl \
	--disable-mshtmlhelp \
	--disable-postscript \
	--disable-printarch \
	--disable-protocol-ftp \
	--disable-ribbon \
	--disable-richtext \
	--disable-sound \
	--disable-static \
	--disable-svg \
	--disable-webkit \
	--disable-webview \
	--disable-xrc \
	--without-expat \
	--without-gtkprint \
	--without-opengl \
	--without-sdl

ZEROMQ_CONFIGURE_OPTIONS = --enable-silent-rules --disable-static
