/* -*- Mode: Java; tab-width: 4; indent-tabs-mode: nil; c-basic-offset: 4 -*- vim:set ts=4 sw=4 sts=4 et: */
package jp.oist.flint.util;

import org.junit.After;
import org.junit.AfterClass;
import org.junit.Before;
import org.junit.BeforeClass;
import org.junit.Test;
import static org.junit.Assert.*;
import java.math.BigDecimal;
import java.text.NumberFormat;

public class RandomizerTest {

    @BeforeClass
    public static void setUpClass() throws Exception {
    }

    @AfterClass
    public static void tearDownClass() throws Exception {
    }

    @Before
    public void setUp() {
    }

    @After
    public void tearDown() {
    }

    @Test
    public void testGenerateAsString() {
        Randomizer r;

        r = new Randomizer(1l, new BigDecimal(0.1), new BigDecimal(-0.1));
        assertEquals("0.046,-0.018,-0.058,-0.033,0.094,-0.099,0.093,0.088,0.089,0.087",
                     r.generateAsString(10));

        r = new Randomizer(1l, new BigDecimal(0.1), new BigDecimal(-0.1));
        NumberFormat nf = NumberFormat.getIntegerInstance();
        nf.setMinimumFractionDigits(1);
        nf.setGroupingUsed(false);
        r.setNumberFormat(nf);
        assertEquals("0.0,-0.0,-0.1,-0.0,0.1,-0.1,0.1,0.1,0.1,0.1",
                     r.generateAsString(10));
    }
}
