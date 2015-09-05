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

    public SimulationDao(File dir) throws SQLException, IOException {
        this("x.db", dir);
    }

    public SimulationDao(String dbName, File dir)
            throws SQLException, IOException {
        super(dbName, dir);

        mTaskList = new HashMap<>();
        mModelPathList = new HashMap<>();
        mTaskList.put(0, null);  // SQLite indexing is based 1.
    }

    private int indexOf(File modelFile, int startIndex, String order) {
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
                    return -1;
                return result.getInt("rowid");
            }
        } catch (SQLException ex) {
            printError(ex.getErrorCode(), ex.getMessage());
            return -1;
        } catch (IOException ex) {
            printError(ex.getMessage());
            return -1;
        }
    }

    public int indexOf(File modelFile) {
        return indexOf(modelFile, 0, "ASC");
    }

    public int indexOf(File modelFile, int fromIndex) {
        return indexOf(modelFile, fromIndex, "ASC");
    }

    public int lastIndexOf(File modelFile) {
        return indexOf(modelFile, 0, "DESC");
    }

    public int getCount() {
        String sql = "SELECT count(ts.rowid) AS count FROM tasks AS ts "
            + "LEFT JOIN models AS ml "
            + "ON ts.model_id = ml.rowid ";
        try (Statement stmt = getConnection().createStatement();
             ResultSet result = stmt.executeQuery(sql)) {
            if (!result.next())
                return -1;

            return result.getInt("count");
        } catch (SQLException ex) {
            printError(ex.getErrorCode(), ex.getMessage());
            return -1;
        } catch (IOException ex) {
            printError(ex.getMessage());
            return -1;
        }
    }

    private Map<Integer, String> getModelPathList() {
        if (mModelPathList != null && mModelPathList.size() > 1)
            return mModelPathList;

        String sql = "SELECT ts.sim_id AS sim_id, ml.model_path AS model_path "
            + "FROM tasks AS ts "
            + "LEFT JOIN models AS ml "
            + "ON ts.model_id = ml.rowid";
        try (Statement stmt = getConnection().createStatement();
             ResultSet result = stmt.executeQuery(sql)) {

            Map<Integer, String> retvals = new HashMap<>();
            retvals.put(0, null);  // SQLite indexing is based 1.
            ResultSetMetaData metaData = result.getMetaData();
            int columnCount = metaData.getColumnCount();
            while (result.next()) {
                int taskId = -1;
                String modelPath = null;
                for (int i=1; i<=columnCount; i++) {
                    String column = metaData.getColumnName(i);
                    if ("sim_id".equals(column)) {
                        taskId = result.getInt(column);
                    } else if ("model_path".equals(column)) {
                        modelPath = result.getString(column);
                    }
                }

                if (taskId > 0 && modelPath != null)
                    retvals.put(taskId, modelPath);
            }

            mModelPathList = retvals;
            return retvals;
        } catch (SQLException ex) {
            printError(ex.getErrorCode(), ex.getMessage());
            return null;
        } catch (IOException ex) {
            printError(ex.getMessage());
            return null;
        }
    }


    public TaskDao obtainTask(File file) {
        int lastIndex = lastIndexOf(file);

        return obtainTask(lastIndex);
    }

    public synchronized TaskDao obtainTask(int taskId) {
        try {
            if (mTaskList.keySet().contains(taskId))
                return mTaskList.get(taskId);

            Map<Integer, String> modelPathList = getModelPathList();

            if (modelPathList == null || modelPathList.size() <= 1
                    || !modelPathList.containsKey(taskId))
                return null;

            TaskDao retval = new TaskDao(taskId, mWorkingDir);
            mTaskList.put(taskId, retval);
            return retval;
        } catch (SQLException | IOException ex) {
            return null;
        }
    }

    public Job obtainJob(int taskId, int jobId) {
        if (mTaskList.containsKey(taskId))
            return mTaskList.get(taskId).obtainJob(jobId);

        TaskDao taskDao = obtainTask(taskId);
        if (taskDao == null)
            return null;

        return taskDao.obtainJob(jobId);
    }
}
