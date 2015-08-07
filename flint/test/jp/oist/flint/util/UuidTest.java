/* -*- Mode: Java; tab-width: 4; indent-tabs-mode: nil; c-basic-offset: 4 -*- vim:set ts=4 sw=4 sts=4 et: */
package jp.oist.flint.util;

import org.junit.After;
import org.junit.AfterClass;
import org.junit.Before;
import org.junit.BeforeClass;
import org.junit.Test;
import static org.junit.Assert.*;

public class UuidTest {

    public UuidTest() {
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
    public void testFromByteArray() {
        final byte[] nil = {
            (byte)0, (byte)0, (byte)0, (byte)0,    (byte)0, (byte)0, (byte)0, (byte)0,
            (byte)0, (byte)0, (byte)0, (byte)0, (byte)0, (byte)0, (byte)0, (byte)0,
        };
        assertEquals("00000000-0000-0000-0000-000000000000", Uuid.fromByteArray(nil));
        final byte[] bytes = {
            (byte)0x56, (byte)0x1f, (byte)0xc6, (byte)0xa2, (byte)0x3c, (byte)0xbc, (byte)0x11, (byte)0xe5,
            (byte)0xa2, (byte)0x99, (byte)0xbf, (byte)0x78, (byte)0xd5, (byte)0x5c, (byte)0xaf, (byte)0xf8,
        };
        assertEquals("561fc6a2-3cbc-11e5-a299-bf78d55caff8", Uuid.fromByteArray(bytes));
    }
}
