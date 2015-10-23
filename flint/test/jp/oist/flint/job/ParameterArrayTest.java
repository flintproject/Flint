/* -*- Mode: Java; tab-width: 4; indent-tabs-mode: nil; c-basic-offset: 4 -*- vim:set ts=4 sw=4 sts=4 et: */
package jp.oist.flint.job;

import java.util.ArrayList;
import org.junit.After;
import org.junit.AfterClass;
import org.junit.Before;
import org.junit.BeforeClass;
import org.junit.Test;
import static org.junit.Assert.*;

public class ParameterArrayTest {

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
    public void testToString() {
        ArrayList<String> pa;

        pa = new ArrayList<>();
        assertEquals("", new ParameterArray(pa).toString());

        pa = new ArrayList<>();
        pa.add("defaultValue=0.0");
        assertEquals("", new ParameterArray(pa).toString());

        pa = new ArrayList<>();
        pa.add("a=1.0");
        pa.add("b=-2.5");
        pa.add("c=3.14");
        assertEquals("a=1.0,b=-2.5,c=3.14", new ParameterArray(pa).toString());
    }
}
