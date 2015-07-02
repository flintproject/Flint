/* -*- Mode: Java; tab-width: 4; indent-tabs-mode: nil; c-basic-offset: 4 -*- vim:set ts=4 sw=4 sts=4 et: */
package jp.oist.flint.dao;

import java.io.File;
import java.io.IOException;
import java.sql.SQLException;
import junit.framework.Assert;
import org.junit.After;
import org.junit.AfterClass;
import org.junit.Before;
import org.junit.BeforeClass;
import org.junit.Test;

public class SimulationDaoTest {

    public SimulationDaoTest() {
    }
    
    @BeforeClass
    public static void setUpClass() {
    }
    
    @AfterClass
    public static void tearDownClass() {
    }
    
    @Before
    public void setUp() {
    }
    
    @After
    public void tearDown() {
    }

    @Test
    public void testIndexOf_File() throws IOException, SQLException {
            String separator = File.separator;
            File resourceDir = new File(System.getProperty("user.dir"), 
                    "test" + separator + "resources" + separator + "testDao");
        try (SimulationDao simulationDao = new SimulationDao(resourceDir)) {
            File modelFile = new File("DBID560_EJN_2012_Pavlides_STN_GP_network_Normal.isml");
            int expResult = 1;
            int result = simulationDao.indexOf(modelFile);
            Assert.assertEquals(expResult, result);

        }
    }

    @Test
    public void testIndexOf_File_int() throws IOException, SQLException {
            String separator = File.separator;
            File resourceDir = new File(System.getProperty("user.dir"), 
                    "test" + separator + "resources" + separator + "testDao");
        try (SimulationDao simulationDao = new SimulationDao(resourceDir)) {
            File modelFile = new File("DBID560_EJN_2012_Pavlides_STN_GP_network_Normal.isml");
            int fromIndex = 1;
            int expResult = 1;
            int result = simulationDao.indexOf(modelFile, fromIndex);
            Assert.assertEquals(expResult, result);
        }
    }

    @Test
    public void testLastIndexOf() throws IOException, SQLException {
            String separator = File.separator;
            File resourceDir = new File(System.getProperty("user.dir"), 
                    "test" + separator + "resources" + separator + "testDao");
        try (SimulationDao simulationDao = new SimulationDao(resourceDir)) {
            File modelFile = new File("DBID560_EJN_2012_Pavlides_STN_GP_network_Normal.isml");
            int expResult = 1;
            int result = simulationDao.lastIndexOf(modelFile);
            Assert.assertEquals(expResult, result);
        }
    }

    @Test
    public void testGetCount() throws IOException, SQLException {
            String separator = File.separator;
            File resourceDir = new File(System.getProperty("user.dir"), 
                    "test" + separator + "resources" + separator + "testDao");
        try (SimulationDao simulationDao = new SimulationDao(resourceDir)) {
            int expResult = 1;
            int result = simulationDao.getCount();
            Assert.assertEquals(expResult, result);
        }
    }

    @Test
    public void testObtainTask_File() throws IOException, SQLException {
            String separator = File.separator;
            File resourceDir = new File(System.getProperty("user.dir"), 
                    "test" + separator + "resources" + separator + "testDao");
        try (SimulationDao simulationDao = new SimulationDao(resourceDir)) {
            File modelFile = new File("DBID560_EJN_2012_Pavlides_STN_GP_network_Normal.isml");
            TaskDao result = simulationDao.obtainTask(modelFile);
            Assert.assertNotNull(result);

            Assert.assertEquals(1, result.getTaskId());

            modelFile = new File("file_not_exist");
            result = simulationDao.obtainTask(modelFile);
            Assert.assertNull(result);
        }
    }

    @Test
    public void testObtainTask_int() throws IOException, SQLException {
            String separator = File.separator;
            File resourceDir = new File(System.getProperty("user.dir"), 
                    "test" + separator + "resources" + separator + "testDao");
        try (SimulationDao simulationDao = new SimulationDao(resourceDir)) {
            int taskId = 1;
            TaskDao result = simulationDao.obtainTask(taskId);
            Assert.assertNotNull(result);

            taskId = 21;
            result = simulationDao.obtainTask(taskId);
            Assert.assertNull(result);
        }
    }

    @Test
    public void testObtainJob() throws IOException, SQLException {
            String separator = File.separator;
            File resourceDir = new File(System.getProperty("user.dir"), 
                    "test" + separator + "resources" + separator + "testDao");
        try (SimulationDao simulationDao = new SimulationDao(resourceDir)) {
            int taskId = 1;
            int jobId = 1;
            JobDao result = simulationDao.obtainJob(taskId, jobId);
            Assert.assertNotNull(result);
        }
    }
}
