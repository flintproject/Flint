/* -*- Mode: Java; tab-width: 4; indent-tabs-mode: nil; c-basic-offset: 4 -*- vim:set ts=4 sw=4 sts=4 et: */
package jp.oist.flint.dao;

import jp.oist.flint.job.Job;
import java.io.File;
import java.io.IOException;
import java.sql.PreparedStatement;
import java.sql.ResultSet;
import java.sql.ResultSetMetaData;
import java.sql.SQLException;
import java.sql.Statement;
import java.util.HashMap;
import java.util.Map;

public class SimulationDao extends DaoObject {

    // TOOD caching the parameters using the cache-key.
    private Map<Integer, TaskDao> mTaskList ;

    private Map<Integer, String> mModelPathList;

    public SimulationDao(File dir) {
        super("x.db", dir);

        mTaskList = new HashMap<>();
        mModelPathList = new HashMap<>();
    }

    private int indexOf(File modelFile, int startIndex, String order)
        throws DaoException, IOException, SQLException {
        String sql = "SELECT ts.rowid AS rowid FROM tasks AS ts "
            + "LEFT JOIN models AS ml "
            + "ON ts.model_id = ml.rowid "
            + "WHERE ml.model_path = ? AND ts.model_id >= ? "
            + "ORDER BY ts.model_id " + order + " "
            + "LIMIT 1";
        try (PreparedStatement stmt = getConnection().prepareStatement(sql)) {
            String modelPath = modelFile.getPath();

            stmt.setString(1, modelPath);
            stmt.setInt(2, startIndex);

            try (ResultSet result = stmt.executeQuery()) {
                if (!result.next())
                    throw new DaoException("result is empty");
                return result.getInt("rowid");
            }
        }
    }

    public int indexOf(File modelFile)
        throws DaoException, IOException, SQLException {
        return indexOf(modelFile, 0, "ASC");
    }

    public int indexOf(File modelFile, int fromIndex)
        throws DaoException, IOException, SQLException {
        return indexOf(modelFile, fromIndex, "ASC");
    }

    public int lastIndexOf(File modelFile)
        throws DaoException, IOException, SQLException {
        return indexOf(modelFile, 0, "DESC");
    }

    public int getCount()
        throws DaoException, IOException, SQLException {
        String sql = "SELECT count(ts.rowid) AS count FROM tasks AS ts "
            + "LEFT JOIN models AS ml "
            + "ON ts.model_id = ml.rowid ";
        try (Statement stmt = getConnection().createStatement();
             ResultSet result = stmt.executeQuery(sql)) {
            if (!result.next())
                throw new DaoException("failed to query: " + sql);
            return result.getInt("count");
        }
    }

    private Map<Integer, String> getModelPathList()
        throws DaoException, IOException, SQLException {
        if (mModelPathList != null && mModelPathList.size() > 1)
            return mModelPathList;

        String sql = "SELECT ts.sim_id AS sim_id, ml.model_path AS model_path "
            + "FROM tasks AS ts "
            + "LEFT JOIN models AS ml "
            + "ON ts.model_id = ml.rowid";
        try (Statement stmt = getConnection().createStatement();
             ResultSet result = stmt.executeQuery(sql)) {

            Map<Integer, String> retvals = new HashMap<>();
            ResultSetMetaData metaData = result.getMetaData();
            int columnCount = metaData.getColumnCount();
            while (result.next()) {
                int taskId = 0;
                String modelPath = null;
                for (int i=1; i<=columnCount; i++) {
                    String column = metaData.getColumnName(i);
                    if ("sim_id".equals(column)) {
                        taskId = result.getInt(column);
                    } else if ("model_path".equals(column)) {
                        modelPath = result.getString(column);
                        if (modelPath == null)
                            throw new DaoException("model_path is NULL");
                    }
                }
                if (taskId <= 0)
                    throw new DaoException("task id is non-positive: " + taskId);
                retvals.put(taskId, modelPath);
            }

            mModelPathList = retvals;
            return retvals;
        }
    }


    public TaskDao obtainTask(File file)
        throws DaoException, IOException, SQLException {
        int lastIndex = lastIndexOf(file);

        return obtainTask(lastIndex);
    }

    public synchronized TaskDao obtainTask(int taskId)
        throws DaoException, IOException, SQLException {
        if (mTaskList.keySet().contains(taskId))
            return mTaskList.get(taskId);

        Map<Integer, String> modelPathList = getModelPathList();
        if (!modelPathList.containsKey(taskId))
            throw new DaoException("no task of task id " + taskId);

        TaskDao retval = new TaskDao(taskId, mWorkingDir);
        mTaskList.put(taskId, retval);
        return retval;
    }

    public Job obtainJob(int taskId, int jobId) throws DaoException, IOException, SQLException {
        if (mTaskList.containsKey(taskId))
            return mTaskList.get(taskId).obtainJob(jobId);

        TaskDao taskDao = obtainTask(taskId);
        return taskDao.obtainJob(jobId);
    }
}
