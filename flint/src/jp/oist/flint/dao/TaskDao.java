/* -*- Mode: Java; tab-width: 4; indent-tabs-mode: nil; c-basic-offset: 4 -*- vim:set ts=4 sw=4 sts=4 et: */
package jp.oist.flint.dao;

import jp.oist.flint.job.Job;
import java.io.File;
import java.io.FileInputStream;
import java.io.FileOutputStream;
import java.io.IOException;
import java.nio.MappedByteBuffer;
import java.nio.channels.FileChannel;
import java.sql.PreparedStatement;
import java.sql.ResultSet;
import java.sql.SQLException;
import java.util.HashMap;
import java.util.Map;

public class TaskDao extends DaoObject {

    private final int mTaskId;

    private final String mModelPath;

    private final File mControlFile;

    private final File mProgressFile;

    /**
     * 0 means that the number of jobs has not been counted.
     */
    private int mCount = 0;

    private FileInputStream mProgressStream;

    private FileChannel mProgressChannel;

    private MappedByteBuffer mProgressBuffer;

    public TaskDao(int taskId, String modelPath, File dir) {
        super("task.db", new File(dir, String.valueOf(taskId)));

        mTaskId = taskId;
        mModelPath = modelPath;
        mControlFile = new File(new File(dir, String.valueOf(taskId)), "control");
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
        try (FileInputStream fis = new FileInputStream(mControlFile)) {
            return fis.read() == 1;
        } catch (IOException ioe) {
            return false;
        }
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
        } catch (DaoException | IOException ex) {
            return false;
        }
    }

    /*
     * Return true if cancellation succeeded, otherwise false.
     */
    public boolean cancel() {
        long length = mControlFile.length();
        if (length == 0) // it does not exist
            return false;
        try (FileOutputStream fos = new FileOutputStream(mControlFile, false)) {
            for (long i=0;i<length;i++)
                fos.write(1);
            return true;
        } catch (IOException ioe) {
            return false;
        }
    }

    public Job obtainJob(int jobId) throws DaoException, IOException {
        assert jobId > 0;

        fetch();
        File workingDir = Job.buildPath(mWorkingDir, jobId);
        if (!workingDir.isDirectory())
            throw new DaoException("no directory for job " + jobId);
        return new Job(mTaskId, workingDir, mProgressBuffer, jobId);
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
            + "LEFT JOIN parameter_samples AS ps "
            + "ON js.ps_id = ps.rowid ";
        StringBuilder sb = new StringBuilder();
        Number[] params = new Number[combination.size()];
        String[] names = combination.keySet()
            .toArray(new String[combination.size()]);
        for (int i=0; i<combination.size(); i++) {
            String name = names[i];
            params[i] = combination.get(names[i]);
            sb.append(String.format("ps.%s = ? AND ", name));
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

    public File getWorkingDir() {
        return mWorkingDir;
    }
}
