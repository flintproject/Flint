/* -*- Mode: Java; tab-width: 4; indent-tabs-mode: nil; c-basic-offset: 4 -*- vim:set ts=4 sw=4 sts=4 et: */
package jp.oist.flint.job;

import java.io.File;
import org.junit.After;
import org.junit.AfterClass;
import org.junit.Before;
import org.junit.BeforeClass;
import org.junit.Test;
import static org.junit.Assert.*;

public class JobTest {

    public JobTest() {
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
    public void testBuildPath() {
		assertEquals("1/00/00/00/00",
					 Job.buildPath(new File("1"), 0).toString());
		assertEquals("foo/100/00/12/d6/87",
					 Job.buildPath(new File("foo/100"), 1234567).toString());
		assertEquals("bar/8/7f/ff/ff/ff",
					 Job.buildPath(new File("bar/8"), 0x7FFFFFFF).toString());
    }
}
