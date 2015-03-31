/* -*- Mode: Java; tab-width: 4; indent-tabs-mode: nil; c-basic-offset: 4 -*- vim:set ts=4 sw=4 sts=4 et: */
package jp.oist.flint.filesystem;

import java.io.File;
import java.io.IOException;
import org.junit.After;
import org.junit.AfterClass;
import org.junit.Before;
import org.junit.BeforeClass;
import org.junit.Test;
import static org.junit.Assert.*;

public class WorkspaceTest {

    public WorkspaceTest() {
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
    public void testCreateTempFile() {
        File f;
        try {
            f = Workspace.createTempFile("", "");
            assertTrue(f.isFile());
            f.deleteOnExit();
            f = Workspace.createTempFile("a", "x");
            assertTrue(f.isFile());
            f.deleteOnExit();
            f = Workspace.createTempFile("ab", "y");
            assertTrue(f.isFile());
            f.deleteOnExit();
            f = Workspace.createTempFile("abc", "xml");
            assertTrue(f.isFile());
            f.deleteOnExit();
            f = Workspace.createTempFile("abcd", "tmp");
            assertTrue(f.isFile());
            f.deleteOnExit();
        } catch (IOException ioe) {
            fail(ioe.getMessage());
        }
    }
}
