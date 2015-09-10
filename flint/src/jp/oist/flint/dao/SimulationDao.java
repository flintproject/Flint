/* -*- Mode: Java; tab-width: 4; indent-tabs-mode: nil; c-basic-offset: 4 -*- vim:set ts=4 sw=4 sts=4 et: */
package jp.oist.flint.dao;

import jp.oist.flint.job.Job;
import java.io.File;
import java.io.IOException;
import java.sql.PreparedStatement;
import java.sql.ResultSet;
import java.sql.SQLException;
import java.util.HashMap;
import java.util.Map;

public class SimulationDao extends DaoObject {

    private Map<Integer, TaskDao> mTaskIdMap;

    private Map<String, TaskDao> mModelPathMap;

    public SimulationDao(File dir) {
        super("x.db", dir);

        mTaskIdMap = null;
        mModelPathMap = null;
    }

    private void fetch()
        throws DaoException, IOException, SQLException {
        String sql = "SELECT t.rowid, m.model_path FROM tasks AS t LEFT JOIN models AS m ON t.model_id = m.rowid";
        try (PreparedStatement stmt = getConnection().prepareStatement(sql);
             ResultSet result = stmt.executeQuery()) {
            HashMap<Integer, TaskDao> mapA = new HashMap<>();
            HashMap<String, TaskDao> mapB = new HashMap<>();
            while (result.next()) {
                int taskId = result.getInt(1);
                String modelPath = result.getString(2);
                TaskDao task = new TaskDao(taskId, mWorkingDir);
                mapA.put(taskId, task);
                if (mapB.containsKey(modelPath))
                    throw new DaoException("duplicate tasks with the same model path: " + modelPath);
                mapB.put(modelPath, task);
            }
            mTaskIdMap = mapA;
            mModelPathMap = mapB;
        }
    }

    public synchronized int getCount()
        throws DaoException, IOException, SQLException {
        if (mTaskIdMap == null)
            fetch();
        return mTaskIdMap.size();
    }

    public synchronized TaskDao obtainTask(String modelPath)
        throws DaoException, IOException, SQLException {
        if (mModelPathMap == null)
            fetch();
        TaskDao task = mModelPathMap.get(modelPath);
        if (task == null)
            throw new DaoException("no task of model path " + modelPath);
        return task;
    }

    public synchronized TaskDao obtainTask(int taskId)
        throws DaoException, IOException, SQLException {
        if (mTaskIdMap == null)
            fetch();
        TaskDao task = mTaskIdMap.get(taskId);
        if (task == null)
            throw new DaoException("no task of task id " + taskId);
        return task;
    }

    public Job obtainJob(int taskId, int jobId) throws DaoException, IOException, SQLException {
        TaskDao taskDao = obtainTask(taskId);
        return taskDao.obtainJob(jobId);
    }
}
