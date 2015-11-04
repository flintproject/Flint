/* -*- Mode: Java; tab-width: 4; indent-tabs-mode: nil; c-basic-offset: 4 -*- vim:set ts=4 sw=4 sts=4 et: */
package jp.oist.flint.dao;

import jp.oist.flint.job.Job;
import java.io.File;
import java.io.IOException;
import java.sql.SQLException;
import java.util.HashMap;
import java.util.Map;
import org.junit.After;
import org.junit.AfterClass;
import org.junit.Before;
import org.junit.BeforeClass;
import org.junit.Test;
import static org.junit.Assert.*;

public class TaskDaoTest {

    SimulationDao mSimulationDao;

    String mWorkingDir;

    public TaskDaoTest() throws IOException {
        String separator = File.separator;
        File resourceDir = new File(System.getProperty("user.dir"), 
                "test" + separator + "resources" + separator + "testDao");
        mWorkingDir = resourceDir.getAbsolutePath();
        mSimulationDao = new SimulationDao(resourceDir);
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
    public void testGetTaskId() throws DaoException, IOException, SQLException {
        int taskId = 1;
        TaskDao task = mSimulationDao.obtainTask(taskId);
        assertEquals(taskId, task.getTaskId());
    }

    @Test
    public void testGetTrackFile() throws DaoException, IOException, SQLException {
        int taskId = 1;
        TaskDao task = mSimulationDao.obtainTask(taskId);
        File trackFile = task.getTrackFile();

        assertNotNull(trackFile);
        String separator = File.separator;
        assertEquals(new File(mWorkingDir + separator + taskId, "track"), trackFile);
    }

    @Test
    public void testObtainJob() throws DaoException, IOException {
        int taskId = 1;
        int jobId = 1;
        TaskDao task = mSimulationDao.obtainTask(taskId);
        Job job = task.obtainJob(jobId);
        assertNotNull(job);
    }

    @Test
    public void testObtainJob__no_such_job_id() throws DaoException, IOException {
        TaskDao task = mSimulationDao.obtainTask(1);
        try {
            task.obtainJob(21);
            fail("exception expected");
        } catch (DaoException de) {
            assertEquals("no directory for job 21", de.getMessage());
        }
    }

    @Test
    public void testIndexOf_NumberArr_StringArr() throws DaoException, IOException, SQLException {
        int taskId = 1;

        Number[] combination = new Number[] { 299.1 };
        String[] titles      = new String[] {"Fs_Ms_Values"};

        TaskDao task = mSimulationDao.obtainTask(taskId);
        assertEquals(11, task.indexOf(combination, titles));

        combination = new Number[] { 0 };
        titles = new String[] {"Fs_Ms_Values"};

        task = mSimulationDao.obtainTask(taskId);
        assertEquals(-1, task.indexOf(combination, titles));

        combination = new Number[] { 299.1 };
        titles = new String[] {"Nothing"};

        task = mSimulationDao.obtainTask(taskId);
        try {
            int i = task.indexOf(combination, titles);
            fail("exception expected: " + i);
        } catch (SQLException e) {
            assertEquals("[SQLITE_ERROR] SQL error or missing database (no such column: e.Nothing)",
                         e.getMessage());
        }

        combination = new Number[] { 0 };
        titles = new String[] {"Nothing"};

        task = mSimulationDao.obtainTask(taskId);
        try {
            int i = task.indexOf(combination, titles);
            fail("exception expected: " + i);
        } catch (SQLException e) {
            assertEquals("[SQLITE_ERROR] SQL error or missing database (no such column: e.Nothing)",
                         e.getMessage());
        }
    }

    @Test
    public void testIndexOf_Map() throws DaoException, IOException, SQLException {
        int taskId = 1;

        Map<String, Number> combination = new HashMap<>();
        combination.put("Fs_Ms_Values", 299.1);

        TaskDao task = mSimulationDao.obtainTask(taskId);
        assertEquals(11, task.indexOf(combination));

        combination.clear();
        combination.put("Fs_Ms_Values", 0);

        task = mSimulationDao.obtainTask(taskId);
        assertEquals(-1, task.indexOf(combination));

        combination.clear();
        combination.put("Nothing", 299.1);

        task = mSimulationDao.obtainTask(taskId);
        try {
            int i = task.indexOf(combination);
            fail("exception expected: " + i);
        } catch (SQLException e) {
            assertEquals("[SQLITE_ERROR] SQL error or missing database (no such column: e.Nothing)",
                         e.getMessage());
        }

        combination.clear();
        combination.put("Nothing", 0);

        task = mSimulationDao.obtainTask(taskId);
        try {
            int i = task.indexOf(combination);
            fail("exception expected: " + i);
        } catch (SQLException e) {
            assertEquals("[SQLITE_ERROR] SQL error or missing database (no such column: e.Nothing)",
                         e.getMessage());
        }
    }

    @Test
    public void testGetCount() throws DaoException, IOException, SQLException {
        int taskId = 1;
        TaskDao task = mSimulationDao.obtainTask(taskId);
        assertEquals(20, task.getCount());
    }
}
