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
