/* -*- Mode: C++; tab-width: 4; indent-tabs-mode: t; c-basic-offset: 4 -*- vim:set ts=4 sw=4 sts=4 noet: */
BOOST_AUTO_TEST_CASE(abs)
{
	TESTCASE1(abs, 0, 0);
	TESTCASE1(abs, 1, 1);
	TESTCASE1(abs, -1, 1);
}

BOOST_AUTO_TEST_CASE(arccos)
{
	TESTCASE1(arccos, 1, 0);
}

BOOST_AUTO_TEST_CASE(arccosh)
{
	TESTCASE1(arccosh, 1, 0);
}

BOOST_AUTO_TEST_CASE(arccot)
{
	TESTCASE1(arccot, 0, 1.5707963267948966); // pi/2
	TESTCASE1(arccot, 1, .785398163397448309); // pi/4
	TESTCASE1(arccot, -1, -.785398163397448309); // -pi/4
}

BOOST_AUTO_TEST_CASE(arccoth)
{
	TESTCASE1(arccoth, 5, .20273255405408219);
	TESTCASE1(arccoth, -5, -.20273255405408219);
}

BOOST_AUTO_TEST_CASE(arccsc)
{
	TESTCASE1(arccsc, 1, 1.5707963267948966); // pi/2
	TESTCASE1(arccsc, -1, -1.5707963267948966); // -pi/2
}

BOOST_AUTO_TEST_CASE(arccsch)
{
	/* TODO */
}

BOOST_AUTO_TEST_CASE(arcsec)
{
	TESTCASE1(arcsec, 1, 0);
}

BOOST_AUTO_TEST_CASE(arcsech)
{
	TESTCASE1(arcsech, 1, 0);
}

BOOST_AUTO_TEST_CASE(arcsin)
{
	TESTCASE1(arcsin, 0, 0);
}

BOOST_AUTO_TEST_CASE(arcsinh)
{
	TESTCASE1(arcsinh, 0, 0);
}

BOOST_AUTO_TEST_CASE(arctan)
{
	TESTCASE1(arctan, 0, 0);
}

BOOST_AUTO_TEST_CASE(arctanh)
{
	TESTCASE1(arctanh, 0, 0);
}

BOOST_AUTO_TEST_CASE(ceiling)
{
	TESTCASE1(ceiling, 0, 0);
	TESTCASE1(ceiling, 1, 1);
	TESTCASE1(ceiling, 1.5, 2);
	TESTCASE1(ceiling, -0.8, 0);
	TESTCASE1(ceiling, -1, -1);
}

BOOST_AUTO_TEST_CASE(cos)
{
	TESTCASE1(cos, 0, 1);
}

BOOST_AUTO_TEST_CASE(cosh)
{
	TESTCASE1(cosh, 0, 1);
}

BOOST_AUTO_TEST_CASE(cot)
{
	/* TODO */
}

BOOST_AUTO_TEST_CASE(coth)
{
	/* TODO */
}

BOOST_AUTO_TEST_CASE(csc)
{
	/* TODO */
}

BOOST_AUTO_TEST_CASE(csch)
{
	/* TODO */
}

BOOST_AUTO_TEST_CASE(divide)
{
	TESTCASE2(divide, 1, 2, 0.5);
	TESTCASE2(divide, -1, 2, -0.5);
	TESTCASE2(divide, 1, -2, -0.5);
	TESTCASE2(divide, -1, -2, 0.5);
}

BOOST_AUTO_TEST_CASE(eq)
{
	TESTCASE2(eq, 0, 0, 1);
	TESTCASE2(eq, 0, 1, 0);
	TESTCASE2(eq, 1, 0, 0);
	TESTCASE2(eq, 1, 1, 1);
}

BOOST_AUTO_TEST_CASE(exp)
{
	TESTCASE1(exp, 0, 1);
}

BOOST_AUTO_TEST_CASE(factorial)
{
	TESTCASE1(factorial, 1, 1);
	TESTCASE1(factorial, 3, 6);
	TESTCASE1(factorial, 10, 3628800);
}

BOOST_AUTO_TEST_CASE(floor)
{
	TESTCASE1(floor, 0, 0);
	TESTCASE1(floor, 0.5, 0);
	TESTCASE1(floor, 1, 1);
	TESTCASE1(floor, -1, -1);
	TESTCASE1(floor, -1.2, -2);
}

BOOST_AUTO_TEST_CASE(geq)
{
	TESTCASE2(geq, 0, 0, 1);
	TESTCASE2(geq, 0, 1, 0);
	TESTCASE2(geq, 1, 0, 1);
	TESTCASE2(geq, 1, 1, 1);
}

BOOST_AUTO_TEST_CASE(gt)
{
	TESTCASE2(gt, 0, 0, 0);
	TESTCASE2(gt, 0, 1, 0);
	TESTCASE2(gt, 1, 0, 1);
	TESTCASE2(gt, 1, 1, 0);
}

BOOST_AUTO_TEST_CASE(leq)
{
	TESTCASE2(leq, 0, 0, 1);
	TESTCASE2(leq, 0, 1, 1);
	TESTCASE2(leq, 1, 0, 0);
	TESTCASE2(leq, 1, 1, 1);
}

BOOST_AUTO_TEST_CASE(ln)
{
	TESTCASE1(ln, 1, 0);
}

BOOST_AUTO_TEST_CASE(log10)
{
	TESTCASE1(log10, 1, 0);
	TESTCASE1(log10, 10, 1);
	TESTCASE1(log10, 100, 2);
	TESTCASE1(log10, 0.1, -1);
	TESTCASE1(log10, 0.01, -2);
}

BOOST_AUTO_TEST_CASE(log)
{
	TESTCASE2(log, 2, 1, 0);
	TESTCASE2(log, 2, 2, 1);
	TESTCASE2(log, 2, 8, 3);
	TESTCASE2(log, 2, 0.5, -1);
	TESTCASE2(log, 3, 1, 0);
	TESTCASE2(log, 3, 9, 2);
}

BOOST_AUTO_TEST_CASE(lt)
{
	TESTCASE2(lt, 0, 0, 0);
	TESTCASE2(lt, 0, 1, 1);
	TESTCASE2(lt, 1, 0, 0);
	TESTCASE2(lt, 1, 1, 0);
}

BOOST_AUTO_TEST_CASE(max)
{
	TESTCASE2(max, 0, 0, 0);
	TESTCASE2(max, 0, 1, 1);
	TESTCASE2(max, 1, 0, 1);
	TESTCASE2(max, 1, 1, 1);
}

BOOST_AUTO_TEST_CASE(min)
{
	TESTCASE2(min, 0, 0, 0);
	TESTCASE2(min, 0, 1, 0);
	TESTCASE2(min, 1, 0, 0);
	TESTCASE2(min, 1, 1, 1);
}

BOOST_AUTO_TEST_CASE(neq)
{
	TESTCASE2(neq, 0, 0, 0);
	TESTCASE2(neq, 0, 1, 1);
	TESTCASE2(neq, 1, 0, 1);
	TESTCASE2(neq, 1, 1, 0);
}

BOOST_AUTO_TEST_CASE(plus)
{
	TESTCASE2(plus, 0, 0, 0);
	TESTCASE2(plus, 0, 1, 1);
	TESTCASE2(plus, 1, 0, 1);
	TESTCASE2(plus, 1, -1, 0);
	TESTCASE2(plus, 1, 2, 3);
	TESTCASE2(plus, -1, -2, -3);
}

BOOST_AUTO_TEST_CASE(power)
{
	TESTCASE2(power, 0, 0, 1);
	TESTCASE2(power, 0, 1, 0);
	TESTCASE2(power, 1, 0, 1);
	TESTCASE2(power, 1, 1, 1);
	TESTCASE2(power, 2, 3, 8);
}

BOOST_AUTO_TEST_CASE(rem)
{
	TESTCASE2(rem, 0, 1, 0);
	TESTCASE2(rem, 0, 2, 0);
	TESTCASE2(rem, 1, 1, 0);
	TESTCASE2(rem, 1, 2, 1);
	TESTCASE2(rem, 2, 3, 2);
	TESTCASE2(rem, 3, 2, 1);
}

BOOST_AUTO_TEST_CASE(root)
{
	TESTCASE1(root, 1, 1);
	TESTCASE1(root, 81, 9);
	TESTCASE1(root, 1/4, 0.5);
	TESTCASE1(root, 9/100, 0.3);
}

BOOST_AUTO_TEST_CASE(sec)
{
	TESTCASE1(sec, 0, 1);
}

BOOST_AUTO_TEST_CASE(sech)
{
	TESTCASE1(sech, 0, 1);
}

BOOST_AUTO_TEST_CASE(sin)
{
	TESTCASE1(sin, 0, 0);
}

BOOST_AUTO_TEST_CASE(sinh)
{
	TESTCASE1(sinh, 0, 0);
}

BOOST_AUTO_TEST_CASE(tan)
{
	TESTCASE1(tan, 0, 0);
}

BOOST_AUTO_TEST_CASE(tanh)
{
	TESTCASE1(tanh, 0, 0);
}

BOOST_AUTO_TEST_CASE(times)
{
	TESTCASE2(times, 0, 0, 0);
	TESTCASE2(times, 0, 1, 0);
	TESTCASE2(times, 1, 0, 0);
	TESTCASE2(times, 2, 3, 6);
	TESTCASE2(times, 3, 2, 6);
	TESTCASE2(times, -2, 3, -6);
	TESTCASE2(times, 2, -3, -6);
	TESTCASE2(times, -2, -3, 6);
}

BOOST_AUTO_TEST_CASE($Mod)
{
	TESTCASE2($Mod, 0, 1, 0);
	TESTCASE2($Mod, 8, 3, 2);
	TESTCASE2($Mod, 8, -3, 2);
	TESTCASE2($Mod, -8, 3, 1);
	TESTCASE2($Mod, -8, -3, 1);
	TESTCASE2($Mod, 1, 2, 1);
	TESTCASE2($Mod, 1, -2, 1);
	TESTCASE2($Mod, -1, 2, 1);
	TESTCASE2($Mod, -1, -2, 1);
	TESTCASE2($Mod, 2.25, 1, 0.25);
	TESTCASE2($Mod, 7.5, 3.5, 0.5);
	TESTCASE2($Mod, -7.5, 3.5, 3);
	TESTCASE2($Mod, -7.5, 3.5, 3);
}
