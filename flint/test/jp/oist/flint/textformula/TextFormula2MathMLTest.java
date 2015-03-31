/* -*- Mode: Java; tab-width: 4; indent-tabs-mode: nil; c-basic-offset: 4 -*- vim:set ts=4 sw=4 sts=4 et: */
package jp.oist.flint.textformula;

import jp.oist.flint.textformula.analyzer.*;

import org.junit.Test;
import org.junit.AfterClass;
import org.junit.BeforeClass;
import static org.junit.Assert.*;
import javax.xml.parsers.ParserConfigurationException;

public class TextFormula2MathMLTest {

    TextFormula2MathML mParser;

    public TextFormula2MathMLTest () {
        mParser = new TextFormula2MathML();
    }

    @BeforeClass
    public static void testSetup () {
    }

    @AfterClass
    public static void testCleanUp () {
    }


    @Test
    public void testParse_Number()
        throws ParseException, ParserConfigurationException {
        String[] numberExpressions = new String[] {
            "x = 1.215752192E1",
            "x = 1",
            "X = 0.23535",
            "x = 3.0E-8",
            "x1 = 585983",
            "xy = 802.5803",
            "X2 = -1",
            "X2 = -2.8E-7",
            "X3 = 1234E+5",
            "yz_1 = -1234567",
            "_xx = -123.4567",
        };

        testParse(numberExpressions, new String[] {
            "<m:math><m:apply><m:eq/><m:ci>x</m:ci><m:cn>1.215752192E1</m:cn></m:apply></m:math>",
            "<m:math><m:apply><m:eq/><m:ci>x</m:ci><m:cn>1</m:cn></m:apply></m:math>",
            "<m:math><m:apply><m:eq/><m:ci>X</m:ci><m:cn>0.23535</m:cn></m:apply></m:math>",
            "<m:math><m:apply><m:eq/><m:ci>x</m:ci><m:cn>3.0E-8</m:cn></m:apply></m:math>",
            "<m:math><m:apply><m:eq/><m:ci>x1</m:ci><m:cn>585983</m:cn></m:apply></m:math>",
            "<m:math><m:apply><m:eq/><m:ci>xy</m:ci><m:cn>802.5803</m:cn></m:apply></m:math>",
            "<m:math><m:apply><m:eq/><m:ci>X2</m:ci><m:apply><m:minus/><m:cn>1</m:cn></m:apply></m:apply></m:math>",
            "<m:math><m:apply><m:eq/><m:ci>X2</m:ci><m:apply><m:minus/><m:cn>2.8E-7</m:cn></m:apply></m:apply></m:math>",
            "<m:math><m:apply><m:eq/><m:ci>X3</m:ci><m:cn>1234E+5</m:cn></m:apply></m:math>",
            "<m:math><m:apply><m:eq/><m:ci>yz_1</m:ci><m:apply><m:minus/><m:cn>1234567</m:cn></m:apply></m:apply></m:math>",
            "<m:math><m:apply><m:eq/><m:ci>_xx</m:ci><m:apply><m:minus/><m:cn>123.4567</m:cn></m:apply></m:apply></m:math>"
        });
    }

    @Test
    public void testParse_Variable()
        throws ParseException, ParserConfigurationException {
        String[] variableExpressions = new String[] {
            "y = bulldog",
            "y = Dachshund",
            "y = _boxer",
            "y = pomeranian_",
            "y = great_dane",
        };

        testParse(variableExpressions, new String[] {
            "<m:math><m:apply><m:eq/><m:ci>y</m:ci><m:ci>bulldog</m:ci></m:apply></m:math>",
            "<m:math><m:apply><m:eq/><m:ci>y</m:ci><m:ci>Dachshund</m:ci></m:apply></m:math>",
            "<m:math><m:apply><m:eq/><m:ci>y</m:ci><m:ci>_boxer</m:ci></m:apply></m:math>",
            "<m:math><m:apply><m:eq/><m:ci>y</m:ci><m:ci>pomeranian_</m:ci></m:apply></m:math>",
            "<m:math><m:apply><m:eq/><m:ci>y</m:ci><m:ci>great_dane</m:ci></m:apply></m:math>"
        });
    }

    @Test
    public void testParse_Functions()
        throws ParseException, ParserConfigurationException {
        String[] functionExpressions = new String[] {
            "y = log(2)",
            "y = pow(2, 4)",
            "y = pow(x, 4)",
            "y = pow(log(x*2), 2)",
        };
        testParse(functionExpressions, new String[] {
            "<m:math><m:apply><m:eq/><m:ci>y</m:ci><m:apply><m:ln/><m:cn>2</m:cn></m:apply></m:apply></m:math>",
            "<m:math><m:apply><m:eq/><m:ci>y</m:ci><m:apply><m:power/><m:cn>2</m:cn><m:cn>4</m:cn></m:apply></m:apply></m:math>",
            "<m:math><m:apply><m:eq/><m:ci>y</m:ci><m:apply><m:power/><m:ci>x</m:ci><m:cn>4</m:cn></m:apply></m:apply></m:math>",
            "<m:math><m:apply><m:eq/><m:ci>y</m:ci><m:apply><m:power/><m:apply><m:ln/><m:apply><m:times/><m:ci>x</m:ci><m:cn>2</m:cn></m:apply></m:apply><m:cn>2</m:cn></m:apply></m:apply></m:math>"
        });
    }

    @Test
    public void testParse_Condition()
        throws ParseException, ParserConfigurationException {
        String[] conditionExpressions = new String[] {
            "x == 2",
            "x != 2",
            "x > 3",
            "x < 87.0",
            "x >= 3.2",
            "y <= x",
        };
        testParse(conditionExpressions, new String[] {
            "<m:math><m:apply><m:eq/><m:ci>x</m:ci><m:cn>2</m:cn></m:apply></m:math>",
            "<m:math><m:apply><m:neq/><m:ci>x</m:ci><m:cn>2</m:cn></m:apply></m:math>",
            "<m:math><m:apply><m:gt/><m:ci>x</m:ci><m:cn>3</m:cn></m:apply></m:math>",
            "<m:math><m:apply><m:lt/><m:ci>x</m:ci><m:cn>87.0</m:cn></m:apply></m:math>",
            "<m:math><m:apply><m:geq/><m:ci>x</m:ci><m:cn>3.2</m:cn></m:apply></m:math>",
            "<m:math><m:apply><m:leq/><m:ci>y</m:ci><m:ci>x</m:ci></m:apply></m:math>"
        });
    }

    @Test
    public void testParse_Vector()
        throws ParseException, ParserConfigurationException {
        String[] vecterExpressions = new String[] {
            "x1 = {1}",
            "x2 = {-8223}",
            "x3 = {log(10), log(20), log(30)}",
            "x4 = {1,2.3,3,4.4,5,6.8}",
            "x5 = {a,1.1,b,2.2,c,3}",
            "x6 = {a,-1.7,-b,4.8,-c,8.5}",
            "x7 = {pow(x1,3), fabs(x2)}",
        };
        testParse(vecterExpressions, new String[] {
            "<m:math><m:apply><m:eq/><m:ci>x1</m:ci><m:apply><m:vector><m:cn>1</m:cn></m:vector></m:apply></m:apply></m:math>",
            "<m:math><m:apply><m:eq/><m:ci>x2</m:ci><m:apply><m:vector><m:apply><m:minus/><m:cn>8223</m:cn></m:apply></m:vector></m:apply></m:apply></m:math>",
            "<m:math><m:apply><m:eq/><m:ci>x3</m:ci><m:apply><m:vector><m:apply><m:ln/><m:cn>10</m:cn></m:apply><m:apply><m:ln/><m:cn>20</m:cn></m:apply><m:apply><m:ln/><m:cn>30</m:cn></m:apply></m:vector></m:apply></m:apply></m:math>",
            "<m:math><m:apply><m:eq/><m:ci>x4</m:ci><m:apply><m:vector><m:cn>1</m:cn><m:cn>2.3</m:cn><m:cn>3</m:cn><m:cn>4.4</m:cn><m:cn>5</m:cn><m:cn>6.8</m:cn></m:vector></m:apply></m:apply></m:math>",
            "<m:math><m:apply><m:eq/><m:ci>x5</m:ci><m:apply><m:vector><m:ci>a</m:ci><m:cn>1.1</m:cn><m:ci>b</m:ci><m:cn>2.2</m:cn><m:ci>c</m:ci><m:cn>3</m:cn></m:vector></m:apply></m:apply></m:math>",
            "<m:math><m:apply><m:eq/><m:ci>x6</m:ci><m:apply><m:vector><m:ci>a</m:ci><m:apply><m:minus/><m:cn>1.7</m:cn></m:apply><m:apply><m:minus/><m:ci>b</m:ci></m:apply><m:cn>4.8</m:cn><m:apply><m:minus/><m:ci>c</m:ci></m:apply><m:cn>8.5</m:cn></m:vector></m:apply></m:apply></m:math>",
            "<m:math><m:apply><m:eq/><m:ci>x7</m:ci><m:apply><m:vector><m:apply><m:power/><m:ci>x1</m:ci><m:cn>3</m:cn></m:apply><m:apply><m:abs/><m:ci>x2</m:ci></m:apply></m:vector></m:apply></m:apply></m:math>",
        });
    }

    @Test
    public void testParse_Matrix()
        throws ParseException, ParserConfigurationException {
        String[] matrixExpressions = new String[] {
            "{{1,2},{3,2},{a,v}}*{{a,v},{d,d},{1,2}}",
            "{1,2,val2}*{{a},{d},{2}}",
        }; 
        testParse(matrixExpressions, new String[] {
            "<m:math><m:apply><m:times/><m:matrix><m:matrixrow><m:cn>1</m:cn><m:cn>2</m:cn></m:matrixrow><m:matrixrow><m:cn>3</m:cn><m:cn>2</m:cn></m:matrixrow><m:matrixrow><m:ci>a</m:ci><m:ci>v</m:ci></m:matrixrow></m:matrix><m:matrix><m:matrixrow><m:ci>a</m:ci><m:ci>v</m:ci></m:matrixrow><m:matrixrow><m:ci>d</m:ci><m:ci>d</m:ci></m:matrixrow><m:matrixrow><m:cn>1</m:cn><m:cn>2</m:cn></m:matrixrow></m:matrix></m:apply></m:math>",
            "<m:math><m:apply><m:times/><m:apply><m:vector><m:cn>1</m:cn><m:cn>2</m:cn><m:ci>val2</m:ci></m:vector></m:apply><m:matrix><m:matrixrow><m:ci>a</m:ci></m:matrixrow><m:matrixrow><m:ci>d</m:ci></m:matrixrow><m:matrixrow><m:cn>2</m:cn></m:matrixrow></m:matrix></m:apply></m:math>"
        });
    }

    @Test
    public void testParse_Expression()
        throws ParseException, ParserConfigurationException {
        String[] simpleExpressions = new String[] {
            "x = (-b+sqrt(pow(b,2)-4*a*c))/2*a",
            "y = a*pow(x,2)+b*x+c",
            "y = (a + b)/3 * (c + 1)/2",
            "y = (y || z) && (p || q)",
            "y = 3 - 4 * 5 / 6 + 7 + 8",
            "a > -20 &&  -4 <= 6",
            "{1,2,val2}*{{a},{d},{2}}*a",
            "pow(val1, val2) * 5",
            "{{1,2},{3,2},{a,v}}*{{a,v},{d,d},{1,2}}*a",
            "x = sin(M_PI * 3)",
            "z_0 = (-sin(x + pow(y, 0.2)) / pow(-x, 3))--a",        
            "Xy = -(a+b+c)",        
        };
        testParse(simpleExpressions, new String [] {
            "<m:math><m:apply><m:eq/><m:ci>x</m:ci><m:apply><m:times/><m:apply><m:divide/><m:apply><m:plus/><m:apply><m:minus/><m:ci>b</m:ci></m:apply><m:apply><m:root/><m:apply><m:minus/><m:apply><m:power/><m:ci>b</m:ci><m:cn>2</m:cn></m:apply><m:apply><m:times/><m:apply><m:times/><m:cn>4</m:cn><m:ci>a</m:ci></m:apply><m:ci>c</m:ci></m:apply></m:apply></m:apply></m:apply><m:cn>2</m:cn></m:apply><m:ci>a</m:ci></m:apply></m:apply></m:math>",
            "<m:math><m:apply><m:eq/><m:ci>y</m:ci><m:apply><m:plus/><m:apply><m:plus/><m:apply><m:times/><m:ci>a</m:ci><m:apply><m:power/><m:ci>x</m:ci><m:cn>2</m:cn></m:apply></m:apply><m:apply><m:times/><m:ci>b</m:ci><m:ci>x</m:ci></m:apply></m:apply><m:ci>c</m:ci></m:apply></m:apply></m:math>",
            "<m:math><m:apply><m:eq/><m:ci>y</m:ci><m:apply><m:divide/><m:apply><m:times/><m:apply><m:divide/><m:apply><m:plus/><m:ci>a</m:ci><m:ci>b</m:ci></m:apply><m:cn>3</m:cn></m:apply><m:apply><m:plus/><m:ci>c</m:ci><m:cn>1</m:cn></m:apply></m:apply><m:cn>2</m:cn></m:apply></m:apply></m:math>",
            "<m:math><m:apply><m:eq/><m:ci>y</m:ci><m:apply><m:and/><m:apply><m:or/><m:ci>y</m:ci><m:ci>z</m:ci></m:apply><m:apply><m:or/><m:ci>p</m:ci><m:ci>q</m:ci></m:apply></m:apply></m:apply></m:math>",
            "<m:math><m:apply><m:eq/><m:ci>y</m:ci><m:apply><m:plus/><m:apply><m:plus/><m:apply><m:minus/><m:cn>3</m:cn><m:apply><m:divide/><m:apply><m:times/><m:cn>4</m:cn><m:cn>5</m:cn></m:apply><m:cn>6</m:cn></m:apply></m:apply><m:cn>7</m:cn></m:apply><m:cn>8</m:cn></m:apply></m:apply></m:math>",
            "<m:math><m:apply><m:and/><m:apply><m:gt/><m:ci>a</m:ci><m:apply><m:minus/><m:cn>20</m:cn></m:apply></m:apply><m:apply><m:leq/><m:apply><m:minus/><m:cn>4</m:cn></m:apply><m:cn>6</m:cn></m:apply></m:apply></m:math>",
            "<m:math><m:apply><m:times/><m:apply><m:times/><m:apply><m:vector><m:cn>1</m:cn><m:cn>2</m:cn><m:ci>val2</m:ci></m:vector></m:apply><m:matrix><m:matrixrow><m:ci>a</m:ci></m:matrixrow><m:matrixrow><m:ci>d</m:ci></m:matrixrow><m:matrixrow><m:cn>2</m:cn></m:matrixrow></m:matrix></m:apply><m:ci>a</m:ci></m:apply></m:math>",
            "<m:math><m:apply><m:times/><m:apply><m:power/><m:ci>val1</m:ci><m:ci>val2</m:ci></m:apply><m:cn>5</m:cn></m:apply></m:math>",
            "<m:math><m:apply><m:times/><m:apply><m:times/><m:matrix><m:matrixrow><m:cn>1</m:cn><m:cn>2</m:cn></m:matrixrow><m:matrixrow><m:cn>3</m:cn><m:cn>2</m:cn></m:matrixrow><m:matrixrow><m:ci>a</m:ci><m:ci>v</m:ci></m:matrixrow></m:matrix><m:matrix><m:matrixrow><m:ci>a</m:ci><m:ci>v</m:ci></m:matrixrow><m:matrixrow><m:ci>d</m:ci><m:ci>d</m:ci></m:matrixrow><m:matrixrow><m:cn>1</m:cn><m:cn>2</m:cn></m:matrixrow></m:matrix></m:apply><m:ci>a</m:ci></m:apply></m:math>",
            "<m:math><m:apply><m:eq/><m:ci>x</m:ci><m:apply><m:sin/><m:apply><m:times/><m:ci>M_PI</m:ci><m:cn>3</m:cn></m:apply></m:apply></m:apply></m:math>",
            "<m:math><m:apply><m:eq/><m:ci>z_0</m:ci><m:apply><m:minus/><m:apply><m:divide/><m:apply><m:minus/><m:apply><m:sin/><m:apply><m:plus/><m:ci>x</m:ci><m:apply><m:power/><m:ci>y</m:ci><m:cn>0.2</m:cn></m:apply></m:apply></m:apply></m:apply><m:apply><m:power/><m:apply><m:minus/><m:ci>x</m:ci></m:apply><m:cn>3</m:cn></m:apply></m:apply><m:apply><m:minus/><m:ci>a</m:ci></m:apply></m:apply></m:apply></m:math>",
            "<m:math><m:apply><m:eq/><m:ci>Xy</m:ci><m:apply><m:minus/><m:apply><m:plus/><m:apply><m:plus/><m:ci>a</m:ci><m:ci>b</m:ci></m:apply><m:ci>c</m:ci></m:apply></m:apply></m:apply></m:math>"
        });
    }

    private void testParse(String[] expressions, String[] result)
        throws ParseException, ParserConfigurationException {
		assertEquals("length mismatch between expressions and results",
					 expressions.length, result.length);
        for (int i=0; i<result.length; i++) {
            String expr = expressions[i];
            String mathml = result[i];
				mParser.parse(new java.io.StringReader(expr));
				assertEquals(String.format("Failed to parse epxression => %s", expr),
							 mathml,
							 mParser.getMathML());
        }
    }
}
