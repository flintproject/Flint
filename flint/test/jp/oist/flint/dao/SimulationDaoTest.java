/* -*- Mode: Java; tab-width: 4; indent-tabs-mode: nil; c-basic-offset: 4 -*- vim:set ts=4 sw=4 sts=4 et: */
package jp.oist.flint.dao;

import jp.oist.flint.job.Job;
import java.io.File;
import java.io.IOException;
import java.sql.SQLException;
import java.util.Set;
import org.junit.After;
import org.junit.AfterClass;
import org.junit.Before;
import org.junit.BeforeClass;
import org.junit.Test;
import static org.junit.Assert.*;

public class SimulationDaoTest {

    private final File mDir;

    public SimulationDaoTest() {
        String sep = File.separator;
        mDir = new File(System.getProperty("user.dir"), "test" + sep + "resources" + sep + "testDao");
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
    public void testGetTaskIdSet() throws DaoException, IOException, SQLException {
        try (SimulationDao simulationDao = new SimulationDao(mDir)) {
            Set<Integer> result = simulationDao.getTaskIdSet();
            assertEquals(1, result.size());
            assertTrue(result.contains(1));
        }
    }

    @Test
    public void testObtainTask_File() throws DaoException, IOException, SQLException {
        try (SimulationDao simulationDao = new SimulationDao(mDir)) {
            TaskDao result = simulationDao.obtainTask("DBID560_EJN_2012_Pavlides_STN_GP_network_Normal.isml");
            assertNotNull(result);

            assertEquals(1, result.getTaskId());
        }
    }

    @Test
    public void testObtainTask_File__failure() throws IOException, SQLException {
        try (SimulationDao simulationDao = new SimulationDao(mDir)) {
            simulationDao.obtainTask("file_not_exist");
            fail("exception expected");
        } catch (DaoException de) {
            assertEquals("no task of model path file_not_exist", de.getMessage());
        }
    }

    @Test
    public void testObtainTask_int() throws DaoException, IOException, SQLException {
        try (SimulationDao simulationDao = new SimulationDao(mDir)) {
            int taskId = 1;
            TaskDao result = simulationDao.obtainTask(taskId);
            assertNotNull(result);
        }
    }

    @Test
    public void testObtainTask_int__failure() throws IOException, SQLException {
        try (SimulationDao simulationDao = new SimulationDao(mDir)) {
            simulationDao.obtainTask(21);
            fail("exception expected");
        } catch (DaoException de) {
            assertEquals("no task of task id 21", de.getMessage());
        }
    }

    @Test
    public void testObtainJob() throws DaoException, IOException, SQLException {
        try (SimulationDao simulationDao = new SimulationDao(mDir)) {
            int taskId = 1;
            int jobId = 1;
            Job result = simulationDao.obtainJob(taskId, jobId);
            assertNotNull(result);
        }
    }
}
