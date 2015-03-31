/* -*- Mode: Java; tab-width: 4; indent-tabs-mode: nil; c-basic-offset: 4 -*- vim:set ts=4 sw=4 sts=4 et: */
package jp.oist.flint.textformula.mathml;


import java.io.IOException;
import org.junit.Test;
import org.junit.AfterClass;
import org.junit.BeforeClass;
import static org.junit.Assert.*;
import org.xml.sax.SAXException;
import javax.xml.parsers.ParserConfigurationException;

public class MathML2TextFormulaTest {

    MathML2TextFormula mParser;

    public MathML2TextFormulaTest () {
        mParser = new MathML2TextFormula();
    }

    @BeforeClass
    public static void testSetup () {
    }

    @AfterClass
    public static void testCleanUp () {
    }


    @Test
    public void testParse_Number()
        throws IOException, ParserConfigurationException, SAXException {
        String[] numberExpressions = new String[] {
            "x == 1",
            "X == 0.23535",
            "x1 == 585983",
            "xy == 802.5803",
            "X2 == -1",
            "yz_1 == -1234567",
            "_xx == -123.4567",
        };

        String[] mathmlExpressions = new String[] {
            "<math xmlns=\"http://www.w3.org/1998/Math/MathML\"><eq/><ci>x</ci><cn>1</cn></math>",
            "<math xmlns=\"http://www.w3.org/1998/Math/MathML\"><eq/><ci>X</ci><cn>0.23535</cn></math>",
            "<math xmlns=\"http://www.w3.org/1998/Math/MathML\"><eq/><ci>x1</ci><cn>585983</cn></math>",
            "<math xmlns=\"http://www.w3.org/1998/Math/MathML\"><eq/><ci>xy</ci><cn>802.5803</cn></math>",
            "<math xmlns=\"http://www.w3.org/1998/Math/MathML\"><eq/><ci>X2</ci><cn>-1</cn></math>",
            "<math xmlns=\"http://www.w3.org/1998/Math/MathML\"><eq/><ci>yz_1</ci><cn>-1234567</cn></math>",
            "<math xmlns=\"http://www.w3.org/1998/Math/MathML\"><eq/><ci>_xx</ci><cn>-123.4567</cn></math>",
        };

        testParse(mathmlExpressions, numberExpressions);
    }

    @Test
    public void testParse_Variable()
        throws IOException, ParserConfigurationException, SAXException {
        String[] variableExpressions = new String[] {
            "y == bulldog",
            "y == Dachshund",
            "y == _boxer",
            "y == pomeranian_",
            "y == great_dane",
        };

        String[] mathmlExpressions = new String[] {
            "<math xmlns=\"http://www.w3.org/1998/Math/MathML\"><eq/><ci>y</ci><ci>bulldog</ci></math>",
            "<math xmlns=\"http://www.w3.org/1998/Math/MathML\"><eq/><ci>y</ci><ci>Dachshund</ci></math>",
            "<math xmlns=\"http://www.w3.org/1998/Math/MathML\"><eq/><ci>y</ci><ci>_boxer</ci></math>",
            "<math xmlns=\"http://www.w3.org/1998/Math/MathML\"><eq/><ci>y</ci><ci>pomeranian_</ci></math>",
            "<math xmlns=\"http://www.w3.org/1998/Math/MathML\"><eq/><ci>y</ci><ci>great_dane</ci></math>",
        };
        testParse(mathmlExpressions, variableExpressions);
    }

    @Test
    public void testParse_Functions()
        throws IOException, ParserConfigurationException, SAXException {
        String[] functionExpressions = new String[] {
            "y == ln(2)",
            "y == pow(2, 4)",
            "y == pow(x, 4)",
            "y == pow(ln(x * 2), 2)",
        };
        String[] mathmlExpressions = new String[] {
            "<math xmlns=\"http://www.w3.org/1998/Math/MathML\"><eq/><ci>y</ci><apply><ln/><cn>2</cn></apply></math>",
            "<math xmlns=\"http://www.w3.org/1998/Math/MathML\"><eq/><ci>y</ci><apply><power/><cn>2</cn><cn>4</cn></apply></math>",
            "<math xmlns=\"http://www.w3.org/1998/Math/MathML\"><eq/><ci>y</ci><apply><power/><ci>x</ci><cn>4</cn></apply></math>",
            "<math xmlns=\"http://www.w3.org/1998/Math/MathML\"><eq/><ci>y</ci><apply><power/><apply><ln/><apply><times/><ci>x</ci><cn>2</cn></apply></apply><cn>2</cn></apply></math>",
        };
        testParse(mathmlExpressions, functionExpressions);
    }

    @Test
    public void testParse_Condition()
        throws IOException, ParserConfigurationException, SAXException {
        String[] conditionExpressions = new String[] {
            "x = 2",
            "x != 2",
            "x > 3",
            "x < 87.0",
            "x >= 3.2",
            "y <= x",
        };
        String[] mathmlExpressions = new String[] {
            "<math xmlns=\"http://www.w3.org/1998/Math/MathML\"><apply><eq/><ci>x</ci><cn>2</cn></apply></math>",
            "<math xmlns=\"http://www.w3.org/1998/Math/MathML\"><apply><neq/><ci>x</ci><cn>2</cn></apply></math>",
            "<math xmlns=\"http://www.w3.org/1998/Math/MathML\"><apply><gt/><ci>x</ci><cn>3</cn></apply></math>",
            "<math xmlns=\"http://www.w3.org/1998/Math/MathML\"><apply><lt/><ci>x</ci><cn>87.0</cn></apply></math>",
            "<math xmlns=\"http://www.w3.org/1998/Math/MathML\"><apply><geq/><ci>x</ci><cn>3.2</cn></apply></math>",
            "<math xmlns=\"http://www.w3.org/1998/Math/MathML\"><apply><leq/><ci>y</ci><ci>x</ci></apply></math>"
        };
        testParse(mathmlExpressions, conditionExpressions);
    }

    @Test
    public void testParse_Vector()
        throws IOException, ParserConfigurationException, SAXException {
        String[] vecterExpressions = new String[] {
            "x1 == {1}",
            "x2 == {-8223}",
            "x3 == {ln(10), ln(20), ln(30)}",
            "x4 == {1, 2.3, 3, 4.4, 5, 6.8}",
            "x5 == {a, 1.1, b, 2.2, c, 3}",
            "x6 == {a, -1.7, -b, 4.8, -c, 8.5}",
            "x7 == {pow(x1, 3), fabs(x2)}",
        };
        String[] mathmlExpressions = new String[] {
            "<math xmlns=\"http://www.w3.org/1998/Math/MathML\"><eq/><ci>x1</ci><apply><vector><cn>1</cn></vector></apply></math>",
            "<math xmlns=\"http://www.w3.org/1998/Math/MathML\"><eq/><ci>x2</ci><apply><vector><apply><minus/><cn>8223</cn></apply></vector></apply></math>",
            "<math xmlns=\"http://www.w3.org/1998/Math/MathML\"><eq/><ci>x3</ci><apply><vector><apply><ln/><cn>10</cn></apply><apply><ln/><cn>20</cn></apply><apply><ln/><cn>30</cn></apply></vector></apply></math>",
            "<math xmlns=\"http://www.w3.org/1998/Math/MathML\"><eq/><ci>x4</ci><apply><vector><cn>1</cn><cn>2.3</cn><cn>3</cn><cn>4.4</cn><cn>5</cn><cn>6.8</cn></vector></apply></math>",
            "<math xmlns=\"http://www.w3.org/1998/Math/MathML\"><eq/><ci>x5</ci><apply><vector><ci>a</ci><cn>1.1</cn><ci>b</ci><cn>2.2</cn><ci>c</ci><cn>3</cn></vector></apply></math>",
            "<math xmlns=\"http://www.w3.org/1998/Math/MathML\"><eq/><ci>x6</ci><apply><vector><ci>a</ci><apply><minus/><cn>1.7</cn></apply><apply><minus/><ci>b</ci></apply><cn>4.8</cn><apply><minus/><ci>c</ci></apply><cn>8.5</cn></vector></apply></math>",
            "<math xmlns=\"http://www.w3.org/1998/Math/MathML\"><eq/><ci>x7</ci><apply><vector><apply><power/><ci>x1</ci><cn>3</cn></apply><apply><abs/><ci>x2</ci></apply></vector></apply></math>"
        };
        testParse(mathmlExpressions, vecterExpressions);
    }

    @Test
    public void testParse_Matrix()
        throws IOException, ParserConfigurationException, SAXException {
        String[] matrixExpressions = new String[] {
            "{{1, 2}, {3, 2}, {a, v}} * {{a, v}, {d, d}, {1, 2}}",
            "{1, 2, val2} * {{a}, {d}, {2}}",
        };

        String[] mathmlExpressions = new String[] {
            "<math xmlns=\"http://www.w3.org/1998/Math/MathML\"><apply><times/><matrix><matrixrow><cn>1</cn><cn>2</cn></matrixrow><matrixrow><cn>3</cn><cn>2</cn></matrixrow><matrixrow><ci>a</ci><ci>v</ci></matrixrow></matrix><matrix><matrixrow><ci>a</ci><ci>v</ci></matrixrow><matrixrow><ci>d</ci><ci>d</ci></matrixrow><matrixrow><cn>1</cn><cn>2</cn></matrixrow></matrix></apply></math>",
            "<math xmlns=\"http://www.w3.org/1998/Math/MathML\"><apply><times/><apply><vector><cn>1</cn><cn>2</cn><ci>val2</ci></vector></apply><matrix><matrixrow><ci>a</ci></matrixrow><matrixrow><ci>d</ci></matrixrow><matrixrow><cn>2</cn></matrixrow></matrix></apply></math>"
        };

        testParse(mathmlExpressions, matrixExpressions);
    }

    @Test
    public void testParse_Expression()
        throws IOException, ParserConfigurationException, SAXException {
        String[] simpleExpressions = new String[] {
            "x == (-b + sqrt(pow(b, 2) - 4 * a * c)) / 2 * a",
            "y == (a * pow(x, 2) + b * x + c)",
            "y == (a + b) / 3 * (c + 1) / 2",
            "y == (y || z) && (p || q)",
            "(a > -20) && (-4 <= 6)",
            "{1, 2, val2} * {{a}, {d}, {2}} * a",
            "pow(val1, val2) * 5",
            "{{1, 2}, {3, 2}, {a, v}} * {{a, v}, {d, d}, {1, 2}} * a",
            "x == sin(M_PI * 3)",
            "z_0 == (-sin(x + pow(y, 0.2)) / pow(-x, 3) - -a)",
            "Xy == -(a + b + c)",
        };

        String[] mathmlExpressions = new String[] {
            "<math xmlns=\"http://www.w3.org/1998/Math/MathML\"><eq/><ci>x</ci><apply><times/><apply><divide/><apply><plus/><apply><minus/><ci>b</ci></apply><apply><root/><apply><minus/><apply><power/><ci>b</ci><cn>2</cn></apply><apply><times/><apply><times/><cn>4</cn><ci>a</ci></apply><ci>c</ci></apply></apply></apply></apply><cn>2</cn></apply><ci>a</ci></apply></math>",

            "<math xmlns=\"http://www.w3.org/1998/Math/MathML\"><eq/><ci>y</ci><apply><plus/><apply><plus/><apply><times/><ci>a</ci><apply><power/><ci>x</ci><cn>2</cn></apply></apply><apply><times/><ci>b</ci><ci>x</ci></apply></apply><ci>c</ci></apply></math>",

            "<math xmlns=\"http://www.w3.org/1998/Math/MathML\"><eq/><ci>y</ci><apply><divide/><apply><times/><apply><divide/><apply><plus/><ci>a</ci><ci>b</ci></apply><cn>3</cn></apply><apply><plus/><ci>c</ci><cn>1</cn></apply></apply><cn>2</cn></apply></math>",

            "<math xmlns=\"http://www.w3.org/1998/Math/MathML\"><eq/><ci>y</ci><apply><and/><apply><or/><ci>y</ci><ci>z</ci></apply><apply><or/><ci>p</ci><ci>q</ci></apply></apply></math>",

            "<math xmlns=\"http://www.w3.org/1998/Math/MathML\"><apply><and/><apply><gt/><ci>a</ci><apply><minus/><cn>20</cn></apply></apply><apply><leq/><apply><minus/><cn>4</cn></apply><cn>6</cn></apply></apply></math>",

            "<math xmlns=\"http://www.w3.org/1998/Math/MathML\"><apply><times/><apply><times/><apply><vector><cn>1</cn><cn>2</cn><ci>val2</ci></vector></apply><matrix><matrixrow><ci>a</ci></matrixrow><matrixrow><ci>d</ci></matrixrow><matrixrow><cn>2</cn></matrixrow></matrix></apply><ci>a</ci></apply></math>",

            "<math xmlns=\"http://www.w3.org/1998/Math/MathML\"><apply><times/><apply><power/><ci>val1</ci><ci>val2</ci></apply><cn>5</cn></apply></math>",

            "<math xmlns=\"http://www.w3.org/1998/Math/MathML\"><apply><times/><apply><times/><matrix><matrixrow><cn>1</cn><cn>2</cn></matrixrow><matrixrow><cn>3</cn><cn>2</cn></matrixrow><matrixrow><ci>a</ci><ci>v</ci></matrixrow></matrix><matrix><matrixrow><ci>a</ci><ci>v</ci></matrixrow><matrixrow><ci>d</ci><ci>d</ci></matrixrow><matrixrow><cn>1</cn><cn>2</cn></matrixrow></matrix></apply><ci>a</ci></apply></math>",

            "<math xmlns=\"http://www.w3.org/1998/Math/MathML\"><eq/><ci>x</ci><apply><sin/><apply><times/><ci>M_PI</ci><cn>3</cn></apply></apply></math>",

            "<math xmlns=\"http://www.w3.org/1998/Math/MathML\"><eq/><ci>z_0</ci><apply><minus/><apply><divide/><apply><minus/><apply><sin/><apply><plus/><ci>x</ci><apply><power/><ci>y</ci><cn>0.2</cn></apply></apply></apply></apply><apply><power/><apply><minus/><ci>x</ci></apply><cn>3</cn></apply></apply><apply><minus/><ci>a</ci></apply></apply></math>",

            "<math xmlns=\"http://www.w3.org/1998/Math/MathML\"><eq/><ci>Xy</ci><apply><minus/><apply><plus/><apply><plus/><ci>a</ci><ci>b</ci></apply><ci>c</ci></apply></apply></math>"
        };
        testParse(mathmlExpressions, simpleExpressions);
    }

    private void testParse(String[] mathmls, String[] result)
        throws IOException, ParserConfigurationException, SAXException {
		assertEquals("length mismatch between mathmls and results",
					 mathmls.length, result.length);
        for (int i=0; i<result.length; i++) {
            String mathml = mathmls[i];
            String expr = result[i];
                mParser.parse(new java.io.StringReader(mathml));
				assertEquals(String.format("Failed to parse epxression => %s", mathml),
							 expr, mParser.getTextFormula());
        }
    }
}
