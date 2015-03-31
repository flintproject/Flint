/* -*- Mode: Java; tab-width: 4; indent-tabs-mode: nil; c-basic-offset: 4 -*- vim:set ts=4 sw=4 sts=4 et: */
package jp.oist.flint.util;

import java.io.File;
import java.math.BigDecimal;
import org.junit.After;
import org.junit.AfterClass;
import org.junit.Before;
import org.junit.BeforeClass;
import org.junit.Test;
import static org.junit.Assert.*;

public class UtilityTest {

    public UtilityTest() {
    }

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
    public void testGetFileName() {
        assertEquals("abc", Utility.getFileName("C:/Users/Foo/bar/abc"));
        assertEquals("abc", Utility.getFileName("C:/Users/Foo/bar/abc.isml"));
        assertEquals("xyz", Utility.getFileName("/Users/Foo/bar/xyz"));
        assertEquals("xyz", Utility.getFileName("/home/foo/bar/xyz.isml"));
    }

    @Test
    public void testGetFileExtension() {
        assertNull(Utility.getFileExtension(new File("abc")));
        assertEquals("", Utility.getFileExtension(new File("abc.")));
        assertEquals("csv", Utility.getFileExtension(new File("/Users/foo/abc.csv")));
        assertEquals("isd", Utility.getFileExtension(new File("C:/Users/bar/abc.csv.isd")));
    }

    @Test
    public void testGetEfficientRound() {
        assertEquals(new BigDecimal(0.1).toString(), Utility.getEfficientRound(new BigDecimal(0.1), 0));
        assertEquals("0.1", Utility.getEfficientRound(new BigDecimal(0.102), 1));
        assertEquals("0.10", Utility.getEfficientRound(new BigDecimal(0.102), 2));
        assertEquals("0.102", Utility.getEfficientRound(new BigDecimal(0.102), 3));
        assertEquals("123.457", Utility.getEfficientRound(new BigDecimal(123.456789), 6));
    }
}
