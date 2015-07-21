/* -*- Mode: Java; tab-width: 4; indent-tabs-mode: nil; c-basic-offset: 4 -*- vim:set ts=4 sw=4 sts=4 et: */
package jp.oist.flint.util;

import org.junit.After;
import org.junit.AfterClass;
import org.junit.Before;
import org.junit.BeforeClass;
import org.junit.Test;
import static org.junit.Assert.*;

public class DurationFormatTest {

    public DurationFormatTest() {
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
    public void testFromMillis() {
        assertEquals("0s", DurationFormat.fromMillis(999));
        assertEquals("4s", DurationFormat.fromMillis(4321));
        assertEquals("1m59s", DurationFormat.fromMillis(60 * 1000 + 59 * 1000));
        assertEquals("12h34m56s", DurationFormat.fromMillis(12 * 60 * 60 * 1000 + 34 * 60 * 1000 + 56 * 1000));
        assertEquals("123h0m0s", DurationFormat.fromMillis(123 * 60 * 60 * 1000));
    }
}
