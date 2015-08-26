/* -*- Mode: Java; tab-width: 4; indent-tabs-mode: nil; c-basic-offset: 4 -*- vim:set ts=4 sw=4 sts=4 et: */
package jp.oist.flint.filesystem;

import java.io.File;
import org.junit.After;
import org.junit.AfterClass;
import org.junit.Before;
import org.junit.BeforeClass;
import org.junit.Test;
import static org.junit.Assert.*;

public class FilenameTest {

    public FilenameTest() {
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
    public void testGetNameWithoutExtension() {
		assertEquals("abc",
					 Filename.getNameWithoutExtension(new File("abc")));
		assertEquals("abc",
					 Filename.getNameWithoutExtension(new File("abc.def")));
		assertEquals("abc.def",
					 Filename.getNameWithoutExtension(new File("abc.def.ghi")));
		assertEquals("foo",
					 Filename.getNameWithoutExtension(new File(System.getProperty("user.dir"), "foo.bar")));
    }

	@Test
	public void testGetVariant() {
		String s = System.getProperty("user.dir");
		File file;
		File dir;
		File result;

		file = new File("foo.txt");
		dir = new File(s);
		result = Filename.getVariant(file, dir, 0, 1, "csv");
		assertEquals(new File(s, "foo_0.csv"), result);

		file = new File(s, "abc");
		dir = new File("xxx");
		result = Filename.getVariant(file, dir, 12, 3, "jar");
		assertEquals(new File("xxx", "abc_012.jar"), result);
	}
}
