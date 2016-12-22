BOOST_UNUSED_COMPONENTS = \
	atomic \
	chrono \
	container \
	context \
	coroutine \
	coroutine2 \
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
	serialization \
	signals \
	thread \
	timer \
	type_erasure \
	wave

B2_WITHOUT_OPTIONS = $(foreach c,$(BOOST_UNUSED_COMPONENTS),--without-$(c))

CLIBSEDML_CONFIGURE_OPTIONS = --enable-silent-rules --disable-static

SOSLIB_CONFIGURE_OPTIONS = \
	--disable-static \
	--without-grace \
	--without-graphviz \
	--without-xerces

LIBSBML_CONFIGURE_OPTIONS = --with-xerces=no

LIBXML2_CONFIGURE_OPTIONS = \
	--disable-static \
	--without-debug \
	--without-ftp \
	--without-http \
	--without-python \
	--without-threads

LIBZIP_CONFIGURE_OPTIONS = --enable-silent-rules --disable-static

PROTOBUF_CONFIGURE_OPTONS = --enable-silent-rules --disable-static

SUNDIALS_CMAKE_OPTIONS = -DBUILD_STATIC_LIBS=OFF -DBUILD_SHARED_LIBS=ON

WXWIDGETS_CONFIGURE_OPTIONS = \
	--enable-cxx11 \
	--disable-compat30 \
	--disable-mediactrl \
	--enable-monolithic
