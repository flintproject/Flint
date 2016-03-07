/* -*- Mode: Java; tab-width: 4; indent-tabs-mode: nil; c-basic-offset: 4 -*- vim:set ts=4 sw=4 sts=4 et: */
package jp.oist.flint.util;

import org.junit.After;
import org.junit.AfterClass;
import org.junit.Before;
import org.junit.BeforeClass;
import org.junit.Test;
import static org.junit.Assert.*;
import java.util.HashMap;

public class LegendCollectionTest {

    public LegendCollectionTest() {
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
    public void testToHashMap() {
		LegendCollection lc = new LegendCollection();
		lc.register("key1", "foo", 10);
		lc.register("key2", "x", 20);
		lc.register("key3", "bar", 30);
		lc.register("key4", "x", 40);
		lc.register("key5", "x", 50);
		HashMap<String, String> m = lc.toHashMap();
        assertEquals(5, m.size());
        assertEquals("foo", m.get("key1"));
        assertEquals("(20) x", m.get("key2"));
        assertEquals("bar", m.get("key3"));
        assertEquals("(40) x", m.get("key4"));
        assertEquals("(50) x", m.get("key5"));
    }
}
