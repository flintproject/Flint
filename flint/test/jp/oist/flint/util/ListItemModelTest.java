/* -*- Mode: Java; tab-width: 4; indent-tabs-mode: nil; c-basic-offset: 4 -*- vim:set ts=4 sw=4 sts=4 et: */
package jp.oist.flint.util;

import java.util.ArrayList;
import java.util.LinkedHashMap;
import org.junit.After;
import org.junit.AfterClass;
import org.junit.Before;
import org.junit.BeforeClass;
import org.junit.Test;
import static org.junit.Assert.*;

public class ListItemModelTest {

    private ListItemModel mModel;

    public ListItemModelTest() {
    }

    @BeforeClass
    public static void setUpClass() throws Exception {
    }

    @AfterClass
    public static void tearDownClass() throws Exception {
    }

    @Before
    public void setUp() {
        LinkedHashMap<String, String> map = new LinkedHashMap<>();
        map.put("x", "X");
        map.put("y", "Y");
        map.put("z", "ZZZ");
        mModel = new ListItemModel(map);
    }

    @After
    public void tearDown() {
    }

    @Test
    public void testGetSize() {
        assertEquals(3, mModel.getSize());
    }

    @Test
    public void testGetElementAt() {
        ListItem x = (ListItem)mModel.getElementAt(0);
        assertEquals("x", x.getKey());
        ListItem y = (ListItem)mModel.getElementAt(1);
        assertEquals("y", y.getKey());
        ListItem z = (ListItem)mModel.getElementAt(2);
        assertEquals("z", z.getKey());
    }

    @Test
    public void testFirstlement() {
        ListItem x = (ListItem)mModel.firstElement();
        assertEquals("x", x.getKey());
    }

    @Test
    public void testAdd() {
        mModel.add("a", "A");
        assertEquals(4, mModel.getSize());
        ListItem a = (ListItem)mModel.getElementAt(3);
        assertEquals("a", a.getKey());
    }

    @Test
    public void testRemove() {
        mModel.remove(1);
        assertEquals(2, mModel.getSize());
        ListItem z = (ListItem)mModel.getElementAt(1);
        assertEquals("z", z.getKey());
    }

    @Test
    public void testKeyAt() {
        assertEquals("y", mModel.keyAt(1));
    }

    @Test
    public void testNameAt() {
        assertEquals("Y", mModel.nameAt(1));
    }

    @Test
    public void testFirstKey() {
        assertEquals("x", mModel.firstKey());
    }

    @Test
    public void testFirstName() {
        assertEquals("X", mModel.firstName());
    }

    @Test
    public void testKeys() {
        ArrayList<String> keys = new ArrayList<>();
        keys.add("x");
        keys.add("y");
        keys.add("z");
        assertEquals(keys, mModel.keys());
    }
}
