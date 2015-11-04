/* -*- Mode: Java; tab-width: 4; indent-tabs-mode: nil; c-basic-offset: 4 -*- vim:set ts=4 sw=4 sts=4 et: */
package jp.oist.flint.dao;

import jp.oist.flint.job.Job;
import java.io.File;
import java.io.FileInputStream;
import java.io.IOException;
import java.nio.MappedByteBuffer;
import java.nio.channels.FileChannel;
import java.sql.PreparedStatement;
import java.sql.ResultSet;
import java.sql.ResultSetMetaData;
import java.sql.SQLException;
import java.util.HashMap;
import java.util.Map;

public class TaskDao extends DaoObject {

    private final int mTaskId;

    private final String mModelPath;

    private final File mProgressFile;

    /**
     * 0 means that the number of jobs has not been counted.
     */
    private int mCount = 0;

    private FileInputStream mProgressStream;

    private FileChannel mProgressChannel;

    private MappedByteBuffer mProgressBuffer;

    public TaskDao(int taskId, String modelPath, File dir) {
        super("db", new File(dir, String.valueOf(taskId)));

        mTaskId = taskId;
        mModelPath = modelPath;
        mProgressFile = new File(new File(dir, String.valueOf(taskId)), "progress");
    }

    public int getTaskId() {
        return mTaskId;
    }

    public String getModelPath() {
        return mModelPath;
    }

    public File getTrackFile() {
        return new File(mWorkingDir, "track");
    }

    public boolean isCancelled() {
        File cancelFile = new File(mWorkingDir, "canceled");
        return cancelFile.exists();
    }

    public boolean isStarted() {
        return mProgressFile.exists();
    }

    public boolean isFinished() {
        if (isCancelled()) return true;
        if (!isStarted()) return false;

        try {
            int jobCount = getCount();
            if (jobCount <= 0) return false;
            for (int i=1; i<=jobCount; i++) {
                Job job = obtainJob(i);
                if (job.isFinished()) continue;
                return false;
            }
            return true;
        } catch (DaoException | IOException | SQLException ex) {
            return false;
        }
    }

    /*
     * Return true if cancellation succeeded, otherwise false.
     */
    public boolean cancel() throws DaoException, IOException, SQLException {
        if (!mWorkingDir.exists())
            return false;
        File cancelFile = new File(mWorkingDir, "canceled");
        if ( !cancelFile.exists() &&
             !cancelFile.createNewFile() )
            return false;

        boolean result = true;
        int jobCount = getCount();
        for (int i=1; i<=jobCount; i++) {
            Job job = obtainJob(i);
            if (!job.cancel())
                result = false;
        }
        return result;
    }

    public Job obtainJob(int jobId) throws DaoException, IOException, SQLException {
        assert jobId > 0;

        fetch();
        File workingDir = Job.buildPath(mWorkingDir, jobId);
        Map<String, Number> combination = getCombination(jobId);
        return new Job(mTaskId, workingDir, combination, mProgressBuffer, jobId);
    }

    public int indexOf(Number[] combination, String[] titles)
        throws IOException, SQLException {
        assert combination.length == titles.length;

        int length = combination.length;

        HashMap<String, Number> map = new HashMap<>();
        for (int i= 0; i<length; i++)
            map.put(titles[i], combination[i]);

        return indexOf(map);
    }

    public int indexOf(Map<String, Number> combination)
        throws IOException, SQLException {
        String sql = "SELECT js.rowid AS rowid FROM jobs AS js "
            + "LEFT JOIN enum AS e "
            + "ON js.enum_id = e.rowid ";
        StringBuilder sb = new StringBuilder();
        Number[] params = new Number[combination.size()];
        String[] names = combination.keySet()
            .toArray(new String[combination.size()]);
        for (int i=0; i<combination.size(); i++) {
            String name = names[i];
            params[i] = combination.get(names[i]);
            sb.append(String.format("e.%s = ? AND ", name));
        }

        String where = sb.substring(0, sb.length() - "AND ".length()) + " ";
        sql = sql + "WHERE " + where + " LIMIT 1" ;

        try (PreparedStatement stmt = getConnection().prepareStatement(sql)) {
            for (int i=0; i<params.length; i++) {
                Number v = params[i];
                if (v instanceof Integer || v instanceof Long) {
                    stmt.setInt(i+1, v.intValue());
                } else {
                    stmt.setDouble(i+1, v.doubleValue());
                }
            }

            try (ResultSet result = stmt.executeQuery()) {
                if (!result.next())
                    return -1;

                return result.getInt("rowid");
            }
        }
    }

    private synchronized void fetch() throws DaoException, IOException {
        if (mCount > 0)
            return;

        int len = (int)mProgressFile.length();
        if (len <= 1)
            throw new DaoException("The progress file is invalid");
        mCount = len-1;
        mProgressStream = new FileInputStream(mProgressFile);
        mProgressChannel = mProgressStream.getChannel();
        mProgressBuffer = mProgressChannel.map(FileChannel.MapMode.READ_ONLY, 0, len);
    }

    public int getCount() throws DaoException, IOException {
        fetch();
        return mCount;
    }

    public int getProgress() throws DaoException, IOException {
        fetch();
        return mProgressBuffer.get(0);
    }

    private Map<String, Number> getCombination(int jobId)
        throws DaoException, IOException, SQLException {
        String sql = "SELECT e.* FROM jobs AS j LEFT JOIN enum AS e ON j.enum_id = e.rowid WHERE j.rowid = ?";
        try (PreparedStatement stmt = getConnection().prepareStatement(sql)) {
            stmt.setInt(1, jobId); // base 1
            try (ResultSet result = stmt.executeQuery()) {
                if (!result.next())
                    throw new DaoException("no job of job-id " + jobId);

                ResultSetMetaData metaData = result.getMetaData();
                int columnCount = metaData.getColumnCount();
                HashMap<String, Number> map = new HashMap<>();
                for (int index=1; index<=columnCount; index++) { // base 1
                    String column = metaData.getColumnName(index);
                    map.put(column, result.getDouble(index));
                }
                return map;
            }
        }
    }

    public File getWorkingDir() {
        return mWorkingDir;
    }
}
