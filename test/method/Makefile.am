include $(top_srcdir)/common.mk
include $(top_srcdir)/test/test.mk

TESTS = \
	test_assign \
	test_euler \
	test_euler_maruyama \
	test_event \
	test_rk4

noinst_PROGRAMS = $(TESTS)

test_assign_SOURCES = test_assign.cc
test_euler_SOURCES = test_euler.cc
test_euler_maruyama_SOURCES = test_euler_maruyama.cc
test_event_SOURCES = test_event.cc
test_rk4_SOURCES = test_rk4.cc
