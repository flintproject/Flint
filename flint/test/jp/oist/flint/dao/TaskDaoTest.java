/* -*- Mode: Java; tab-width: 4; indent-tabs-mode: nil; c-basic-offset: 4 -*- vim:set ts=4 sw=4 sts=4 et: */
package jp.oist.flint.dao;

import java.io.File;
import java.io.IOException;
import java.sql.SQLException;
import java.util.Arrays;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import org.apache.log4j.xml.DOMConfigurator;
import org.junit.After;
import org.junit.AfterClass;
import org.junit.Before;
import org.junit.BeforeClass;
import org.junit.Test;
import org.junit.Assert;

public class TaskDaoTest {

    SimulationDao mSimulationDao;

    String mWorkingDir;

    public TaskDaoTest() throws SQLException, IOException {
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
    public void testGetTaskId() {
        try {
            int taskId = 1;
            TaskDao task = mSimulationDao.obtainTask(taskId);
            Assert.assertEquals(taskId, task.getTaskId());
            Assert.assertEquals(new File("DBID560_EJN_2012_Pavlides_STN_GP_network_Normal.isml"), 
                                task.getModelFile());
        } catch (Exception ex) {
            Assert.fail(String.format("%s : %s", 
                    ex.getClass().getName(), ex.getMessage()));
        }
    }

    @Test
    public void testGetModelFile() {
        try {
            int taskId = 1;
            TaskDao task = mSimulationDao.obtainTask(taskId);
            Assert.assertEquals(taskId, task.getTaskId());
            Assert.assertEquals(new File("DBID560_EJN_2012_Pavlides_STN_GP_network_Normal.isml"), 
                                task.getModelFile());
        } catch (Exception ex) {
            Assert.fail(String.format("%s : %s", 
                    ex.getClass().getName(), ex.getMessage()));
        }
    }

    @Test
    public void testGetTrackFile() {
        try {
            int taskId = 1;
            TaskDao task = mSimulationDao.obtainTask(taskId);
            File trackFile = task.getTrackFile();

            Assert.assertNotNull(trackFile);
            String separator = File.separator;
            Assert.assertEquals(new File(mWorkingDir + separator + taskId, "track"), trackFile);
        } catch (Exception ex) {
            Assert.fail(String.format("%s : %s", 
                    ex.getClass().getName(), ex.getMessage()));
        }
    }

    @Test
    public void testObtainJob() {
        try {
            int taskId = 1;
            int jobId = 1;
            TaskDao task = mSimulationDao.obtainTask(taskId);
            JobDao job = task.obtainJob(jobId);
            Assert.assertNotNull(job);

            jobId = 21;
            job = task.obtainJob(jobId);
            Assert.assertNull(job);
        } catch (Exception ex) {
            Assert.fail(String.format("%s : %s", 
                    ex.getClass().getName(), ex.getMessage()));
        }
    }

    @Test
    public void testIndexOf_NumberArr_StringArr() throws Exception {
        try {
            int taskId = 1;

            Number[] combination = new Number[] { 299.1 };
            String[] titles      = new String[] {"Fs_Ms_Values"};

            TaskDao task = mSimulationDao.obtainTask(taskId);
            Assert.assertEquals(11, task.indexOf(combination, titles));

            combination = new Number[] { 0 };
            titles = new String[] {"Fs_Ms_Values"};

            task = mSimulationDao.obtainTask(taskId);
            Assert.assertEquals(-1, task.indexOf(combination, titles));

            combination = new Number[] { 299.1 };
            titles = new String[] {"Nothing"};

            task = mSimulationDao.obtainTask(taskId);
            Assert.assertEquals(-1, task.indexOf(combination, titles));

            combination = new Number[] { 0 };
            titles = new String[] {"Nothing"};

            task = mSimulationDao.obtainTask(taskId);
            Assert.assertEquals(-1, task.indexOf(combination, titles));

        } catch (Exception ex) {
            Assert.fail(String.format("%s : %s", 
                    ex.getClass().getName(), ex.getMessage()));
        }
    }

    @Test
    public void testIndexOf_Map() {
        try {
            int taskId = 1;

            Map<String, Number> combination = new HashMap<>();
            combination.put("Fs_Ms_Values", 299.1);

            TaskDao task = mSimulationDao.obtainTask(taskId);
            Assert.assertEquals(11, task.indexOf(combination));

            combination.clear();
            combination.put("Fs_Ms_Values", 0);

            task = mSimulationDao.obtainTask(taskId);
            Assert.assertEquals(-1, task.indexOf(combination));

            combination.clear();
            combination.put("Nothing", 299.1);
            
            task = mSimulationDao.obtainTask(taskId);
            Assert.assertEquals(-1, task.indexOf(combination));

            combination.clear();
            combination.put("Nothing", 0);
            
            task = mSimulationDao.obtainTask(taskId);
            Assert.assertEquals(-1, task.indexOf(combination));

        } catch (Exception ex) {
            Assert.fail(String.format("%s : %s", 
                    ex.getClass().getName(), ex.getMessage()));
        }
    }

    @Test
    public void testGetStatus() {
        try {
            int taskId = 1;
            int jobId = 1;
            TaskDao task = mSimulationDao.obtainTask(taskId);
            Assert.assertEquals(TaskDao.Status.GENERATED, task.getStatus(jobId));

            jobId = 11;
            Assert.assertEquals(TaskDao.Status.PENDING, task.getStatus(jobId));
        } catch (Exception ex) {
            Assert.fail(String.format("%s : %s", 
                    ex.getClass().getName(), ex.getMessage()));
        }
    }

    @Test
    public void testGetCount() {
        try {
            int taskId = 1;
            TaskDao task = mSimulationDao.obtainTask(taskId);
            Assert.assertEquals(20, task.getCount());
        } catch (Exception ex) {
            Assert.fail(String.format("%s : %s", 
                    ex.getClass().getName(), ex.getMessage()));
        }
    }

    @Test
    public void testGetPendingCount() {
        try {
            int taskId = 1;
            TaskDao task = mSimulationDao.obtainTask(taskId);
            Assert.assertEquals(10, task.getPendingCount());
        } catch (Exception ex) {
            Assert.fail(String.format("%s : %s", 
                    ex.getClass().getName(), ex.getMessage()));
        }
    }

    @Test
    public void testGetGeneratedCount() {
        try {
            int taskId = 1;
            TaskDao task = mSimulationDao.obtainTask(taskId);
            Assert.assertEquals(10, task.getGeneratedCount());
        } catch (Exception ex) {
            Assert.fail(String.format("%s : %s", 
                    ex.getClass().getName(), ex.getMessage()));
        }
    }

    @Test
    public void testGetProgress() {
        try {
            int taskId = 1;
            TaskDao task = mSimulationDao.obtainTask(taskId);
            Assert.assertEquals(52, task.getProgress());
        } catch (Exception ex) {
            Assert.fail(String.format("%s : %s", 
                    ex.getClass().getName(), ex.getMessage()));
        }
    }

    @Test
    public void testGetIndices() {
        try {
            int taskId = 1;
            TaskDao task = mSimulationDao.obtainTask(taskId);

            List<Integer> actuals = Arrays.asList(new Integer[] {
                1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20
            });
            List<Integer> indices = task.getIndices();
            Assert.assertEquals(indices, actuals);
        } catch (Exception ex) {
            Assert.fail(String.format("%s : %s", 
                    ex.getClass().getName(), ex.getMessage()));
        }
    }

    @Test
    public void testGetIndicesOf() {
        try {
            int taskId = 1;
            TaskDao task = mSimulationDao.obtainTask(taskId);

            List<Integer> indices = task.getIndicesOf(TaskDao.Status.GENERATED);
            List<Integer> actuals = Arrays.asList(new Integer[] {
                1,2,3,4,5,6,7,8,9,10
            });
            Assert.assertEquals(indices, actuals);

            indices = task.getIndicesOf(TaskDao.Status.PENDING);
            actuals = Arrays.asList(new Integer[] {
                11,12,13,14,15,16,17,18,19,20
            });
            Assert.assertEquals(indices, actuals);
        } catch (Exception ex) {
            Assert.fail(String.format("%s : %s", 
                    ex.getClass().getName(), ex.getMessage()));
        }
    }
}
