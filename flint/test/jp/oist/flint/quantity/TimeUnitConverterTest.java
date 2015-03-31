/* -*- Mode: Java; tab-width: 4; indent-tabs-mode: nil; c-basic-offset: 4 -*- vim:set ts=4 sw=4 sts=4 et: */
package jp.oist.flint.quantity;

import jp.physiome.Ipc;
import org.junit.After;
import org.junit.AfterClass;
import org.junit.Before;
import org.junit.BeforeClass;
import org.junit.Test;
import static org.junit.Assert.*;

public class TimeUnitConverterTest {

    public TimeUnitConverterTest() {
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
    public void testConvert() {
        Ipc.TimeUnit.Builder builder = Ipc.TimeUnit.newBuilder();
        Ipc.TimeUnit centi = builder.setName("centi").setD(100).setN(1).build();
        Ipc.TimeUnit milli = builder.setName("milli").setD(1000).setN(1).build();
        assertEquals("0.3", TimeUnitConverter.convert("0.3", centi, centi));
        assertEquals("1.0", TimeUnitConverter.convert("0.1", centi, milli));
        assertEquals("0.25", TimeUnitConverter.convert("2.5", milli, centi));
    }
}
