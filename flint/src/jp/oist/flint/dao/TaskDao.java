/* -*- Mode: Java; tab-width: 4; indent-tabs-mode: nil; c-basic-offset: 4 -*- vim:set ts=4 sw=4 sts=4 et: */
package jp.oist.flint.dao;

import jp.oist.flint.job.Job;
import jp.oist.flint.job.Progress;
import java.io.File;
import java.io.FilenameFilter;
import java.io.IOException;
import java.sql.PreparedStatement;
import java.sql.ResultSet;
import java.sql.ResultSetMetaData;
import java.sql.SQLException;
import java.util.HashMap;
import java.util.Map;

public class TaskDao extends DaoObject {

    private final int mTaskId;

    /**
     * 0 means that the number of jobs has not been counted.
     */
    private int mCount = 0;

    public TaskDao(int taskId, File dir) throws SQLException {
        super("db", new File(dir, String.valueOf(taskId)));

        mTaskId = taskId;
    }

    public int getTaskId() {
        return mTaskId;
    }

    public File getTrackFile() {
        return new File(mWorkingDir, "track");
    }

    public boolean isCancelled() {
        File cancelFile = new File(mWorkingDir, "canceled");
        return cancelFile.exists();
    }

    private static class JobDirNameFilter implements FilenameFilter {
        @Override
        public boolean accept(File dir, String name) {
            return name.matches("^[1-9][0-9]*$");
        }
    }

    public boolean isStarted() {
        File[] jobDirs = mWorkingDir.listFiles(new JobDirNameFilter());
        return jobDirs.length > 0;
    }

    public boolean isFinished() {
        if (!isStarted()) return false;
        if (isCancelled()) return true;

        try {
            int jobCount = getCount();
            if (jobCount <= 0) return false;
            for (int i=1; i<=jobCount; i++) {
                Job job = obtainJob(i);
                if (job == null) return false;
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
            if (job == null || !job.cancel())
                result = false;
        }
        return result;
    }

    public Job obtainJob(int jobId) throws DaoException, IOException, SQLException {
        assert jobId > 0;

        File workingDir = new File(mWorkingDir, String.valueOf(jobId));
        Map<String, Number> combination = getCombination(jobId);
        return new Job(mTaskId, workingDir, combination, jobId);
    }

    public int indexOf(Number[] combination, String[] titles)
            throws IOException {

        if (combination.length != titles.length)
            throw new IOException("");

        int length = combination.length;

        HashMap<String, Number> map = new HashMap<>();
        for (int i= 0; i<length; i++)
            map.put(titles[i], combination[i]);

        return indexOf(map);
    }

    public int indexOf(Map<String, Number> combination) {
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
        } catch (SQLException ex) {
            printError(ex.getErrorCode(), ex.getMessage());
            return -1;
        } catch (IOException ex) {
            printError(ex.getMessage());
            return -1;
        }
    }

    public synchronized int getCount() throws DaoException, IOException, SQLException {
        if (mCount > 0)
            return mCount;

        String sql = "SELECT count(*) FROM jobs";
        try (PreparedStatement stmt = getConnection().prepareStatement(sql);
             ResultSet result = stmt.executeQuery()) {
            if (!result.next())
                throw new DaoException("failed to query: " + sql);
            mCount = result.getInt(1);
            return mCount;
        }
    }

    public int getProgress() {
        try {
            int jobCount = getCount();
            int total = 0;
            for (int i=1; i<=jobCount; i++) {
                Job job = obtainJob(i);
                if (job == null)
                    continue;
                Progress progress = job.getProgress();
                int p = progress.getPercent();
                if (p <= 0)
                    continue;
                total += p;
            }
            return (int)((double)total / (double)jobCount);
        } catch (DaoException | IOException | SQLException ex) {
            return 0;
        }
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
}
