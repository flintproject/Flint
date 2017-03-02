/* -*- Mode: C++; tab-width: 4; indent-tabs-mode: t; c-basic-offset: 4 -*- vim:set ts=4 sw=4 sts=4 noet: */
#ifdef HAVE_CONFIG_H
#include "config.h"
#endif

#include "phml.h"

#define BOOST_FILESYSTEM_NO_DEPRECATED
#include <boost/filesystem.hpp>
#include "database.h"

#define BOOST_TEST_MODULE test_read
#include "test.h"

class F : public test::MemoryFixture {
public:
	F()
		: sql_(driver_.db())
	{
		// start working at the temporary directory
		boost::filesystem::create_directory("tmp");
		boost::filesystem::current_path("tmp");
	}

	~F() {
		// cleanup the working directory
		boost::filesystem::current_path("..");
		boost::filesystem::remove_all("tmp");
	}

	void ReadAndCheck(const char *file)
	{
		BOOST_REQUIRE_EQUAL(SaveGivenFile(driver_.db(), file), 1);
		BOOST_CHECK(phml::Read(driver_.db()));
	}

	void CheckModules(const std::vector<std::string> &expected)
	{
		sql_.CheckRows("SELECT lower(hex(module_id)), name FROM modules",
					   expected);
	}

	void CheckModules(const char *expected)
	{
		CheckModules(FromFile(expected));
	}

	void CheckPqs(const std::vector<std::string> &expected)
	{
		sql_.CheckRows("SELECT lower(hex(m.module_id)), p.type, p.pq_id, p.name FROM pqs AS p LEFT JOIN modules AS m ON p.module_rowid = m.rowid",
					   expected);
	}

	void CheckPqs(const char *expected)
	{
		CheckPqs(FromFile(expected));
	}

	void CheckImpls(const std::vector<std::string> &expected)
	{
		sql_.CheckRows("SELECT lower(hex(m.module_id)), p.name||i.math FROM impls AS i LEFT JOIN pqs AS p ON i.pq_rowid = p.rowid LEFT JOIN modules AS m ON p.module_rowid = m.rowid",
					   expected);
	}

	void CheckExtras(const std::vector<std::string> &expected)
	{
		sql_.CheckRows("SELECT lower(hex(m.module_id)), p.name, e.order_type||e.math FROM extras AS e LEFT JOIN pqs AS p ON e.pq_rowid = p.rowid LEFT JOIN modules AS m ON p.module_rowid = m.rowid",
					   expected);
	}

	void CheckIvs(const std::vector<std::string> &expected)
	{
		sql_.CheckRows("SELECT lower(hex(m.module_id)), p.name||i.math FROM ivs AS i LEFT JOIN pqs AS p ON i.pq_rowid = p.rowid LEFT JOIN modules AS m ON p.module_rowid = m.rowid",
					   expected);
	}

	void CheckBridges(const std::vector<std::string> &expected)
	{
		sql_.CheckRows("SELECT lower(hex(m.module_id)), p.pq_id, b.direction, b.sub_type, b.connector FROM bridges AS b LEFT JOIN pqs AS p ON b.pq_rowid = p.rowid LEFT JOIN modules AS m ON p.module_rowid = m.rowid",
					   expected);
	}

	void CheckRefports(const std::vector<std::string> &expected)
	{
		sql_.CheckTable("refports", expected);
	}

	void CheckIports(const std::vector<std::string> &expected)
	{
		sql_.CheckRows("SELECT lower(hex(m.module_id))||':'||r.port_id, p.pq_id FROM refports AS r LEFT JOIN pqs AS p ON r.pq_rowid = p.rowid LEFT JOIN modules AS m ON p.module_rowid = m.rowid",
					   expected);
	}

	void CheckIports(const char *expected)
	{
		CheckIports(FromFile(expected));
	}

	void CheckOports(const std::vector<std::string> &expected)
	{
		sql_.CheckRows("SELECT lower(hex(m.module_id))||':'||p.port_id, p.ref_pq_id FROM ports AS p LEFT JOIN modules AS m ON p.module_rowid = m.rowid WHERE p.direction = 'out'",
					   expected);
	}

	void CheckOports(const char *expected)
	{
		CheckOports(FromFile(expected));
	}

	void CheckEdges(const std::vector<std::string> &expected)
	{
		sql_.CheckRows("SELECT lower(hex(tail_module_id))||':'||tail_port_id, lower(hex(head_module_id))||':'||head_port_id FROM edges",
					   expected);
	}

	void CheckEdges(const char *expected)
	{
		CheckEdges(FromFile(expected));
	}

	void CheckTpqs(const std::vector<std::string> &expected)
	{
		sql_.CheckRows("SELECT lower(hex(instances.module_id)), lower(hex(tms.module_id)), tpqs.pq_id, ltrim(tpqs.math) FROM tpqs LEFT JOIN tms ON tpqs.tm_rowid = tms.rowid LEFT JOIN instances ON tms.instance_rowid = instances.rowid",
					   expected);
	}

	void CheckTpqs(const char *expected)
	{
		CheckTpqs(FromFile(expected));
	}

	void CheckTs(const std::vector<std::string> &expected)
	{
		sql_.CheckRows("SELECT lower(hex(m.module_id)), t.timeseries_id, t.format FROM timeseries AS t LEFT JOIN modules AS m ON t.module_rowid = m.rowid",
					   expected);
	}

	void CheckTsref(const std::vector<std::string> &expected)
	{
		sql_.CheckRows("SELECT lower(hex(m.module_id)), p.pq_id, r.timeseries_id, r.element_id FROM refts AS r LEFT JOIN pqs AS p ON r.pq_rowid = p.rowid LEFT JOIN modules AS m ON p.module_rowid = m.rowid",
					   expected);
	}

	void CheckTrees(const std::vector<std::string> &expected)
	{
		sql_.CheckRows("SELECT lower(hex(module_id)), level FROM trees", expected);
	}

	void CheckTrees(const char *expected)
	{
		CheckTrees(FromFile(expected));
	}

	void CheckScopes(const std::vector<std::string> &expected)
	{
		sql_.CheckRows("SELECT lower(hex(uuid)), lower(hex(space_id)), label FROM scopes", expected);
	}

	void CheckScopes(const char *expected)
	{
		CheckScopes(FromFile(expected));
	}

	void CheckJournals(const std::vector<std::string> &expected)
	{
		sql_.CheckRows("SELECT replace(hex(zeroblob(indent)),'00',' ')||lower(hex(uuid)) FROM journals",
					   expected);
	}

	void CheckJournals(const char *expected)
	{
		CheckJournals(FromFile(expected));
	}

	void CheckSpans(const std::vector<std::string> &expected)
	{
		sql_.CheckRows("SELECT lower(hex(tail_uuid)), tail_port_id, lower(hex(head_uuid)), head_port_id FROM spans", expected);
	}

	void CheckSpans(const char *expected)
	{
		CheckSpans(FromFile(expected));
	}

	void CheckReaches(const std::vector<std::string> &expected)
	{
		sql_.CheckRows("SELECT lower(hex(output_uuid)), output_id, lower(hex(input_uuid)), input_id FROM reaches",
					   expected);
	}

	void CheckReaches(const char *expected)
	{
		CheckReaches(FromFile(expected));
	}

	void CheckFlows(const char *expected)
	{
		sql_.CheckRows("SELECT source, target FROM flows",
					   FromFile(expected));
	}

	void ReadAndError(const char *file, const char *expected)
	{
		BOOST_REQUIRE_EQUAL(SaveGivenFile(driver_.db(), file), 1);
		test::StderrCapture capture;
		BOOST_CHECK(!phml::Read(driver_.db()));
		BOOST_CHECK_EQUAL(capture.Get(), expected);
	}

private:
	std::vector<std::string> FromFile(const char *name)
	{
		boost::filesystem::path sp(__FILE__);
		boost::filesystem::path fp = sp.parent_path();
		fp /= "test-read";
		fp /= name;
		boost::filesystem::ifstream ifs(fp);
		BOOST_REQUIRE(ifs.is_open());
		std::vector<std::string> v;
		std::string line;
		while (std::getline(ifs, line))
			v.push_back(line);
		ifs.close();
		return v;
	}

	test::Sql sql_;
};

BOOST_FIXTURE_TEST_SUITE(test_read, F)

BOOST_AUTO_TEST_CASE(damped_system)
{
	ReadAndCheck(TEST_MODELS("damped-system.phml"));
	std::vector<std::string> ivs{
		"77495f8880884fcbbf2991df656981be v (eq %v (vector 0 0 0 0))",
		"77495f8880884fcbbf2991df656981be x (eq %x (vector 0.1 0 0 0))"
	};
	CheckIvs(ivs);
}

BOOST_AUTO_TEST_CASE(fem1ode)
{
	ReadAndCheck(TEST_MODELS("fem1ode.phml"));
	std::vector<std::string> ivs{
		"f7893575fba740a1b489ef7a1adc3e4d c (eq %c (vector (sin %h) (sin (times 2 %h)) (sin (times 3 %h)) (sin (times 4 %h)) (sin (times 5 %h)) (sin (times 6 %h)) (sin (times 7 %h)) (sin (times 8 %h)) (sin (times 9 %h)) (sin (times 10 %h)) (sin (times 11 %h)) (sin (times 12 %h)) (sin (times 13 %h)) (sin (times 14 %h)) (sin (times 15 %h)) (sin (times 16 %h)) (sin (times 17 %h)) (sin (times 18 %h)) (sin (times 19 %h))))"
	};
	CheckIvs(ivs);
}

BOOST_AUTO_TEST_CASE(fppp_rtm0)
{
	ReadAndCheck(TEST_MODELS("fppp-rtm0.phml"));
}

BOOST_AUTO_TEST_CASE(fppp_rtm1)
{
	ReadAndCheck(TEST_MODELS("fppp-rtm1.phml"));
}

BOOST_AUTO_TEST_CASE(graph_condition)
{
	ReadAndCheck(TEST_MODELS("graph-condition.phml"));
	std::vector<std::string> extras{
		"531f5aeac96111e4a5fea74d28f4c4e3 y before (eq %y (piecewise (piece (piecewise (piece 2 (geq %x 0.5))) (eq %y 1)) (piece (piecewise (piece 3 (leq %x 0))) (eq %y 2)) (piece (piecewise (piece 2 (gt %x 0)) (piece 4 (geq %x 0.5))) (eq %y 3))))"
	};
	CheckExtras(extras);
	std::vector<std::string> impls{
		"531f5aeac96111e4a5fea74d28f4c4e3 x (eq %x (sin %time))",
		"531f5aeac96111e4a5fea74d28f4c4e3 z (eq (diff (bvar %time) %z) (piecewise (piece -1 (eq %y 1)) (piece -2 (eq %y 2)) (piece -3 (eq %y 3)) (otherwise 0)))",
		"531f5aeac96111e4a5fea74d28f4c4e3 y (eq (diff (bvar %time) %y) 0)"
	};
	CheckImpls(impls);
	std::vector<std::string> ivs{
		"531f5aeac96111e4a5fea74d28f4c4e3 y (eq %y 1)",
		"531f5aeac96111e4a5fea74d28f4c4e3 z (eq %z 0)"
	};
	CheckIvs(ivs);
}

BOOST_AUTO_TEST_CASE(graph_instance)
{
	ReadAndCheck(TEST_MODELS("graph-instance.phml"));
	std::vector<std::string> extras{
		"4314d24f375c43098211f869ab0a27ad x after (case-set (case (condition (eq %x 4)) (eq %x 1)))",
		"4314d24f375c43098211f869ab0a27ad x before (eq %x (piecewise (piece (piecewise (piece ($trial ($outcome 2 0.1)) (eq %x 1)) (piece ($trial ($outcome 3 0.01)) (eq %x 2)) (piece ($trial ($outcome 4 0.001)) (eq %x 3))) (eq (rem %time 2) 0))))"
	};
	CheckExtras(extras);
	std::vector<std::string> impls{
		"4314d24f375c43098211f869ab0a27ad x (eq (diff (bvar %time) %x) 0)"
	};
	CheckImpls(impls);
	std::vector<std::string> ivs{
		"4314d24f375c43098211f869ab0a27ad x (eq %x 1)"
	};
	CheckIvs(ivs);
}

BOOST_AUTO_TEST_CASE(graph_probability)
{
	ReadAndCheck(TEST_MODELS("graph-probability.phml"));
	std::vector<std::string> extras{
		"0df0882ecbb711e48be8b31a3f8cd0c1 x before (eq %x (piecewise (piece (piecewise (piece ($trial ($outcome 2 %p)) (eq %x 1)) (piece ($trial ($outcome 3 0.50)) (eq %x 2)) (piece ($trial ($outcome 2 0.49) ($outcome 4 0.01)) (eq %x 3))) (geq %y -1))))"
	};
	CheckExtras(extras);
	std::vector<std::string> impls{
		"0df0882ecbb711e48be8b31a3f8cd0c1 y (eq (diff (bvar %time) %y) (piecewise (piece -1 (eq %x 1)) (piece -2 (eq %x 2)) (piece -3 (eq %x 3)) (otherwise 0)))",
		"0df0882ecbb711e48be8b31a3f8cd0c1 p (eq %p 0.5)",
		"0df0882ecbb711e48be8b31a3f8cd0c1 x (eq (diff (bvar %time) %x) 0)"
	};
	CheckImpls(impls);
	std::vector<std::string> ivs{
		"0df0882ecbb711e48be8b31a3f8cd0c1 x (eq %x 1)",
		"0df0882ecbb711e48be8b31a3f8cd0c1 y (eq %y 0)"
	};
	CheckIvs(ivs);
}

BOOST_AUTO_TEST_CASE(hepatocyte_external)
{
	ReadAndCheck(TEST_MODELS("hepatocyte_external.isml"));
	std::vector<std::string> bridges{
		"4d96c8ded10a48e2a0e0be9d74e58e78 4 get parameter k1",
		"4d96c8ded10a48e2a0e0be9d74e58e78 5 set species s1"
	};
	CheckBridges(bridges);
}

BOOST_AUTO_TEST_CASE(hepatocyte_internal)
{
	ReadAndCheck(TEST_MODELS("hepatocyte_internal.isml"));
	std::vector<std::string> bridges{
		"4d96c8ded10a48e2a0e0be9d74e58e78 4 get parameter k1",
		"4d96c8ded10a48e2a0e0be9d74e58e78 5 set species s1"
	};
	CheckBridges(bridges);
}

BOOST_AUTO_TEST_CASE(Izhikevich_2003_model)
{
	ReadAndCheck(TEST_MODELS("Izhikevich_2003_model.isml"));
	std::vector<std::string> bridges; // empty
	CheckBridges(bridges);
	std::vector<std::string> modules{
		"e92c7a906bf04c35817dfd9505050830 membrane potential",
		"055edf9e8eb24812abb2e3e6f5be12e1 Membrane recovery variable",
		"917c40a328814b6e8d5c3fff0145a898 current_pulse_generator_membrane",
		"b342ed563a214c079299f10ce7806cc7 capsule_of_Membrane recovery variable",
		"2cf24f1b4e2241a88010d132e86fd6e5 capsule_of_current_pulse_generator_membrane"
	};
	CheckModules(modules);
	std::vector<std::string> pqs{
		"e92c7a906bf04c35817dfd9505050830 x 2 v",
		"e92c7a906bf04c35817dfd9505050830 s 3 c",
		"e92c7a906bf04c35817dfd9505050830 v 4 u",
		"e92c7a906bf04c35817dfd9505050830 v 5 Iext",
		"055edf9e8eb24812abb2e3e6f5be12e1 x 2 u",
		"055edf9e8eb24812abb2e3e6f5be12e1 s 3 a",
		"055edf9e8eb24812abb2e3e6f5be12e1 s 4 b",
		"055edf9e8eb24812abb2e3e6f5be12e1 s 5 d",
		"055edf9e8eb24812abb2e3e6f5be12e1 v 6 v",
		"917c40a328814b6e8d5c3fff0145a898 s 2 pulse_onset_time",
		"917c40a328814b6e8d5c3fff0145a898 s 3 pulse_width",
		"917c40a328814b6e8d5c3fff0145a898 s 4 pulse_height",
		"917c40a328814b6e8d5c3fff0145a898 s 5 initial_current",
		"917c40a328814b6e8d5c3fff0145a898 v 6 stimulus_current"
	};
	CheckPqs(pqs);
	std::vector<std::string> trees{
		"e92c7a906bf04c35817dfd9505050830 1",
		"055edf9e8eb24812abb2e3e6f5be12e1 1",
		"b342ed563a214c079299f10ce7806cc7 0",
		"917c40a328814b6e8d5c3fff0145a898 1",
		"2cf24f1b4e2241a88010d132e86fd6e5 0"
	};
	CheckTrees(trees);
}

BOOST_AUTO_TEST_CASE(lemming)
{
	ReadAndCheck(TEST_MODELS("lemming.phml"));
}

BOOST_AUTO_TEST_CASE(LR_pulse_189_164)
{
	ReadAndCheck(TEST_MODELS("LR-pulse-189-164.isml"));
	CheckPqs("LR-pulse-189-164.pqs.txt");
}

BOOST_AUTO_TEST_CASE(lognormal)
{
	ReadAndCheck(TEST_MODELS("lognormal.phml"));
	std::vector<std::string> impls{
		"9f3ff5b62b5311e586cf1b4225aa123b x (eq %x ($lognormal_variate 0 1))",
	};
	CheckImpls(impls);
}

BOOST_AUTO_TEST_CASE(reduction)
{
	ReadAndCheck(TEST_MODELS("reduction.phml"));
	std::vector<std::string> refports{
		"2 1 1",
		"3 1 2",
		"4 1 3",
		"5 1 4",
		"6 1 5",
		"7 1 1",
		"8 2 1",
	};
	CheckRefports(refports);
	CheckReaches("reduction.reaches.txt");
}

BOOST_AUTO_TEST_CASE(rigidode)
{
	ReadAndCheck(TEST_MODELS("rigidode.phml"));
	std::vector<std::string> impls{
		"20439f740c34436d9d414cd2bc18e613 y1 (eq (diff (bvar %time) %y1) (times %y2 %y3))",
		"20439f740c34436d9d414cd2bc18e613 y2 (eq (diff (bvar %time) %y2) (minus (times %y1 %y3)))",
		"20439f740c34436d9d414cd2bc18e613 y3 (eq (diff (bvar %time) %y3) (times (minus %a) %y1 %y2))",
		"20439f740c34436d9d414cd2bc18e613 a (eq %a 0.51)"
	};
	CheckImpls(impls);
}

BOOST_AUTO_TEST_CASE(ringed_Beeler_Reuter_1977_model_with_static_instance)
{
	ReadAndCheck(TEST_MODELS("ringed_Beeler_Reuter_1977_model_with_static_instance.isml"));
	CheckEdges("ringed_Beeler_Reuter_1977_model_with_static_instance.edges.txt");
	CheckFlows("ringed_Beeler_Reuter_1977_model_with_static_instance.flows.txt");
	CheckIports("ringed_Beeler_Reuter_1977_model_with_static_instance.iports.txt");
	CheckJournals("ringed_Beeler_Reuter_1977_model_with_static_instance.journals.txt");
	CheckModules("ringed_Beeler_Reuter_1977_model_with_static_instance.modules.txt");
	CheckOports("ringed_Beeler_Reuter_1977_model_with_static_instance.oports.txt");
	CheckPqs("ringed_Beeler_Reuter_1977_model_with_static_instance.pqs.txt");
	CheckReaches("ringed_Beeler_Reuter_1977_model_with_static_instance.reaches.txt");
	CheckScopes("ringed_Beeler_Reuter_1977_model_with_static_instance.scopes.txt");
	CheckSpans("ringed_Beeler_Reuter_1977_model_with_static_instance.spans.txt");
	std::vector<std::string> tpqs; // empty
	CheckTpqs(tpqs);
	CheckTrees("ringed_Beeler_Reuter_1977_model_with_static_instance.trees.txt");
}

BOOST_AUTO_TEST_CASE(Rybak_2006_with_static_instance_and_multiple_input)
{
	ReadAndCheck(TEST_MODELS("Rybak_2006_with_static_instance_and_multiple_input.isml"));
	CheckEdges("Rybak_2006_with_static_instance_and_multiple_input.edges.txt");
	CheckFlows("Rybak_2006_with_static_instance_and_multiple_input.flows.txt");
	CheckIports("Rybak_2006_with_static_instance_and_multiple_input.iports.txt");
	CheckJournals("Rybak_2006_with_static_instance_and_multiple_input.journals.txt");
	CheckModules("Rybak_2006_with_static_instance_and_multiple_input.modules.txt");
	CheckOports("Rybak_2006_with_static_instance_and_multiple_input.oports.txt");
	CheckPqs("Rybak_2006_with_static_instance_and_multiple_input.pqs.txt");
	CheckReaches("Rybak_2006_with_static_instance_and_multiple_input.reaches.txt");
	CheckScopes("Rybak_2006_with_static_instance_and_multiple_input.scopes.txt");
	CheckSpans("Rybak_2006_with_static_instance_and_multiple_input.spans.txt");
	CheckTpqs("Rybak_2006_with_static_instance_and_multiple_input.tpqs.txt");
	CheckTrees("Rybak_2006_with_static_instance_and_multiple_input.trees.txt");
}

BOOST_AUTO_TEST_CASE(target)
{
	ReadAndCheck(TEST_MODELS("target.phml"));
	std::vector<std::string> journals{
		"   5f5960dced604928b73210139a28d73e",
		"  39d5bc1f380a475186634142149c6bbe",
		"   e4e77b3994f04615a34dfc8b6c447016",
		"  8e425c12147b48328faf70c66ae8bfa2",
		"   412dd5ccbfd4456f90ada853a22f717d",
		"  df1c7a9e33954b2b9553537cd669ce98",
		" c19a428252b24a93ad5656e9b801dfe7",
		"21835270877a4211be239fbcc7aaa9a0"
	};
	CheckJournals(journals);
	std::vector<std::string> scopes{
		"5f5960dced604928b73210139a28d73e c19a428252b24a93ad5656e9b801dfe7 1",
		"39d5bc1f380a475186634142149c6bbe 21835270877a4211be239fbcc7aaa9a0 1",
		"e4e77b3994f04615a34dfc8b6c447016 c19a428252b24a93ad5656e9b801dfe7 2",
		"8e425c12147b48328faf70c66ae8bfa2 21835270877a4211be239fbcc7aaa9a0 2",
		"412dd5ccbfd4456f90ada853a22f717d c19a428252b24a93ad5656e9b801dfe7 3",
		"df1c7a9e33954b2b9553537cd669ce98 21835270877a4211be239fbcc7aaa9a0 3"
	};
	CheckScopes(scopes);
	std::vector<std::string> tpqs{
		"8e425c12147b48328faf70c66ae8bfa2 c19a428252b24a93ad5656e9b801dfe7 1 (eq %b 2)",
		"df1c7a9e33954b2b9553537cd669ce98 c19a428252b24a93ad5656e9b801dfe7 2 (eq %alpha 0.5)"
	};
	CheckTpqs(tpqs);
}

BOOST_AUTO_TEST_CASE(template_test_no_instance_no_edge)
{
	ReadAndCheck(TEST_MODELS("template-test_no-instance_no-edge.phml"));
	std::vector<std::string> journals; // empty
	CheckJournals(journals);
	std::vector<std::string> scopes{
		"507d51c9e2034f4ebc0ea1cc30e5b017 507d51c9e2034f4ebc0ea1cc30e5b017 "
	};
	CheckScopes(scopes);
}

BOOST_AUTO_TEST_CASE(template_test_with_instace)
{
	ReadAndCheck(TEST_MODELS("template-test_with-instace.phml"));
	std::vector<std::string> journals{
		"   549a677de72241e69bb6c7fd9b0ce13b",
		"  25b75eb5d6ff475abbad49be925eddbe",
		" 0856f36112284a9ca601a0b35eb84528",
		"36ea9dd1fab743b1aa546a656cedf0d3"
	};
	CheckJournals(journals);
	std::vector<std::string> scopes{
		"507d51c9e2034f4ebc0ea1cc30e5b017 507d51c9e2034f4ebc0ea1cc30e5b017 ",
		"549a677de72241e69bb6c7fd9b0ce13b 0856f36112284a9ca601a0b35eb84528 ",
		"25b75eb5d6ff475abbad49be925eddbe 36ea9dd1fab743b1aa546a656cedf0d3 "
	};
	CheckScopes(scopes);
}

BOOST_AUTO_TEST_CASE(timeseries)
{
	ReadAndCheck(TEST_MODELS("timeseries.phml"));
	std::vector<std::string> pqs{
		"66d517ba818b11e3899b2ffb02188a45 t 1 a",
		"66d517ba818b11e3899b2ffb02188a45 t 2 b",
		"66d517ba818b11e3899b2ffb02188a45 v 3 c"
	};
	CheckPqs(pqs);
	std::vector<std::string> ts{
		"66d517ba818b11e3899b2ffb02188a45 1 isd",
		"66d517ba818b11e3899b2ffb02188a45 2 csv"
	};
	CheckTs(ts);
	std::vector<std::string> tsref{
		"66d517ba818b11e3899b2ffb02188a45 1 1 x",
		"66d517ba818b11e3899b2ffb02188a45 2 2 y"
	};
	CheckTsref(tsref);
}

BOOST_AUTO_TEST_CASE(double_pendulum) {
	ReadAndError(TEST_MODELS("double_pendulum.isml"),
				 "missing <col> in <dimension> of type vector\n"
				 " at physical-quantity \"sol\" (5) of module \"Dynamic_Equation\" (4f33ece0-83f3-4b0d-8859-96d6f9838929)\n");
}

BOOST_AUTO_TEST_CASE(self_circular_unit) {
	ReadAndError(TEST_MODELS("self-circular-unit.phml"),
				 "unit with unit-id 8 is ill-defined by <element> with its own unit-id\n");
}

BOOST_AUTO_TEST_CASE(state_to_static) {
	ReadAndError(TEST_MODELS("state-to-static.phml"),
				 "found invalid edge from a state to a static-parameter\n"
				 "  from\n"
				 "    port-id: 1\n"
				 "    module-id: 3e10b8f4-75f8-11e4-ae2e-d39dff277ccb\n"
				 "  to\n"
				 "    port-id: 1\n"
				 "    module-id: 3cb504a6-75f8-11e4-85b6-8b3365a37bec\n");
}

BOOST_AUTO_TEST_CASE(static_to_static)
{
	ReadAndCheck(TEST_MODELS("static-to-static.phml"));
	std::vector<std::string> refports{
		"2 1 1",
		"4 1 1",
		"5 2 1"
	};
	CheckRefports(refports);
}

BOOST_AUTO_TEST_CASE(swapped_definitions) {
	ReadAndError(TEST_MODELS("swapped-definitions.phml"),
				 "invalid definition of <implementation>\n"
				 " at physical-quantity \"y\" (2) of module \"M\" (22ddc4e8-c6ff-11e4-a78c-576e48c58a72)\n");
}

BOOST_AUTO_TEST_CASE(variable_to_static) {
	ReadAndError(TEST_MODELS("variable-to-static.phml"),
				 "found invalid edge from a variable-parameter to a static-parameter\n"
				 "  from\n"
				 "    port-id: 1\n"
				 "    module-id: 85ded370-75f6-11e4-ac26-ff6c3d89c805\n"
				 "  to\n"
				 "    port-id: 1\n"
				 "    module-id: 57a14bc8-75f6-11e4-aa25-37030d28703a\n");
}

BOOST_AUTO_TEST_CASE(vdpode)
{
	ReadAndCheck(TEST_MODELS("vdpode.phml"));
	std::vector<std::string> impls{
		"a3148ad1c5004447a26b418c482e2589 y1 (eq (diff (bvar %time) %y1) %y2)",
		"a3148ad1c5004447a26b418c482e2589 y2 (eq (diff (bvar %time) %y2) (minus (times %mu (minus 1 (power %y1 2)) %y2) %y1))",
		"a3148ad1c5004447a26b418c482e2589 mu (eq %mu 1000)"
	};
	CheckImpls(impls);
}

BOOST_AUTO_TEST_CASE(weibull)
{
	ReadAndCheck(TEST_MODELS("weibull.phml"));
	std::vector<std::string> impls{
		"b5a6cbc42b5b11e5ad314f1d7174b098 x (eq %x ($weibull_variate 1 3))",
	};
	CheckImpls(impls);
}

BOOST_AUTO_TEST_CASE(x_capsulated_by) {
	ReadAndError(TEST_MODELS("x-capsulated-by.phml"),
				 "module of module-id daa7dafe-7641-4280-9559-2e080e93578b is capsulated by unknown capsule module: 9114256a-d9bf-11e4-8933-5b24fd24d827\n");
}

BOOST_AUTO_TEST_CASE(x_delay_without_max_delay) {
	ReadAndError(TEST_MODELS("x-delay-without-max-delay.phml"),
				 "a is given as 1st argument of Delay()/DeltaTime(), but it lacks <max-delay>\n"
				 " in fe9114f8-2f6a-11e5-a753-031605a77f16\n");
}

BOOST_AUTO_TEST_CASE(x_empty_definition_of_instantaneous_extra_impl) {
	ReadAndError(TEST_MODELS("x-empty-definition-of-instantaneous-extra-impl.phml"),
				 "empty <definition> in <extra-implementation> of type instantaneous\n"
				 " at physical-quantity \"x\" (1) of module \"X\" (2563a638-89dc-11e5-b0ba-1bdda966072b)\n");
}

BOOST_AUTO_TEST_CASE(x_missing_edge) {
	ReadAndError(TEST_MODELS("x-missing-edge.phml"),
				 "there is no edge to a port;\n"
				 "  port-id: 1\n"
				 "  module-id: 99894d9c-4d11-4e4e-8877-a64ade7007ed\n"
				 "  uuid: 99894d9c-4d11-4e4e-8877-a64ade7007ed\n");
}

BOOST_AUTO_TEST_CASE(x_variable_defined_by_ode) {
	ReadAndError(TEST_MODELS("x-variable-defined-by-ode.phml"),
				 "physical-quantity \"v\" (1) of module \"M\" (b3dee97b-99ff-435e-ada0-fe0861272757) is of type variable-parameter, but defined with <diff>\n");
}

BOOST_AUTO_TEST_CASE(x_variable_with_initial_value) {
	ReadAndError(TEST_MODELS("x-variable-with-initial-value.phml"),
				 "unexpected <initial-value> for <physical-quantity> of type variable-parameter\n"
				 " at physical-quantity \"v\" (1) of module \"X\" (1b432bab-9798-4c6e-ae72-9dfebae97bc1)\n");
}

BOOST_AUTO_TEST_SUITE_END()
