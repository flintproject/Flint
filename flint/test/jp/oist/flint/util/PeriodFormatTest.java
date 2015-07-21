/* -*- Mode: Java; tab-width: 4; indent-tabs-mode: nil; c-basic-offset: 4 -*- vim:set ts=4 sw=4 sts=4 et: */
package jp.oist.flint.util;

import org.junit.After;
import org.junit.AfterClass;
import org.junit.Before;
import org.junit.BeforeClass;
import org.junit.Test;
import static org.junit.Assert.*;
import java.util.Calendar;

public class PeriodFormatTest {

    public PeriodFormatTest() {
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
    public void testFromTo() {
        Calendar from = Calendar.getInstance();
        Calendar to = Calendar.getInstance();
        from.set(2015, 7, 21, 0, 0, 0);
        to.set(2015, 7, 21, 0, 0, 1);
        assertEquals("00:00:00 - 00:00:01",
                     PeriodFormat.fromTo(from.getTime(), to.getTime()));
        from.set(2015, 0, 2, 14, 50, 12);
        to.set(2015, 11, 30, 8, 9, 0);
        assertEquals("2015-01-02 14:50:12 - 2015-12-30 08:09:00",
                     PeriodFormat.fromTo(from.getTime(), to.getTime()));
    }
}
