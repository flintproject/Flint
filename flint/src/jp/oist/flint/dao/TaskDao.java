/* -*- Mode: Java; tab-width: 4; indent-tabs-mode: nil; c-basic-offset: 4 -*- vim:set ts=4 sw=4 sts=4 et: */
package jp.oist.flint.dao;

import jp.oist.flint.job.Progress;
import java.io.File;
import java.io.FilenameFilter;
import java.io.IOException;
import java.sql.PreparedStatement;
import java.sql.ResultSet;
import java.sql.ResultSetMetaData;
import java.sql.SQLException;
import java.sql.Statement;
import java.util.ArrayList;
import java.util.Collections;
import java.util.HashMap;
import java.util.List;
import java.util.Locale;
import java.util.Map;

public class TaskDao extends DaoObject {

    public enum Status {
        PENDING, GENERATED, UNKNOWN;

        public static Status fromString (String s) {
            if (s == null || s.isEmpty())
                return UNKNOWN;
            switch (s.toLowerCase(Locale.ENGLISH)) {
            case "pending":
                return PENDING;
            case "generated":
                return GENERATED;
            }
            return UNKNOWN;
        }
    }

    private List<Map<String, Number>> mCombinations;

    private final int mTaskId;

    private final File mModelFile;

    private int mCount = -1;

    public TaskDao(int taskId, File dir, File modelFile)
            throws SQLException, IOException {
        super("db", new File(dir, String.valueOf(taskId)));

        mTaskId = taskId;
        mModelFile = modelFile;
    }

    public int getTaskId() {
        return mTaskId;
    }

    public File getModelFile() {
        return mModelFile;
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

        int jobCount = getCount();
        if (jobCount <= 0) return false;
        for (int i=1; i<=jobCount; i++) {
            JobDao job = obtainJob(i);
            if (job == null) return false;
            if (job.isFinished()) continue;
            return false;
        }
        return true;
    }

    /*
     * Return true if cancellation succeeded, otherwise false.
     */
    public boolean cancel() throws IOException {
        if (!mWorkingDir.exists())
            return false;
        File cancelFile = new File(mWorkingDir, "canceled");
        if ( !cancelFile.exists() &&
             !cancelFile.createNewFile() )
            return false;

        boolean result = true;
        int jobCount = getCount();
        for (int i=1; i<=jobCount; i++) {
            JobDao job = obtainJob(i);
            if (job == null || !job.cancel())
                result = false;
        }
        return result;
    }

    public JobDao obtainJob(int jobId) {
        getCombinationList();

        if (jobId < 1 || mCombinations == null || jobId >= mCombinations.size())
            return null;

        Map<String, Number> combination = mCombinations.get(jobId);
        File workingDir = new File(mWorkingDir, String.valueOf(jobId));

        return new JobDao(this, workingDir, combination, jobId);
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

    public Status getStatus(int index) {
        String sql = "SELECT js.status AS status FROM jobs AS js "
            + "LEFT JOIN enum AS e "
            + "ON js.enum_id = e.rowid "
            + "WHERE js.rowid = ? LIMIT 1";
        try (PreparedStatement stmt = getConnection().prepareStatement(sql)) {
            stmt.setInt(1, index);
            try (ResultSet result = stmt.executeQuery()) {
                if (!result.next())
                    return Status.UNKNOWN;

                return Status.fromString(result.getString("status"));
            }
        } catch (SQLException ex) {
            printError(ex.getErrorCode(), ex.getMessage());
        } catch (IOException ex) {
            printError(ex.getMessage());
        }
        return Status.UNKNOWN;
    }

    private int getCount(Condition where) {

        if (mCount > 0)
            return mCount;

        String sql = "SELECT count(js.rowid) AS count FROM jobs AS js "
            + "LEFT JOIN enum AS e "
            + "ON js.enum_id = e.rowid ";
        if (where != null)
            sql += "WHERE " + where.toString();

        try (PreparedStatement stmt = getConnection().prepareStatement(sql);
             ResultSet result = stmt.executeQuery()) {
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

    public int getCount() {
        if (mCount <= 0)
            mCount = getCount(null);

        return mCount;
    }

    public int getProgress() {
        int jobCount = getCount();
        if (jobCount <= 0)
            return 0;

        int total = 0;
        for (int i=1; i<=jobCount; i++) {
            JobDao job = obtainJob(i);
            if (job == null)
                continue;
            Progress progress = job.getProgress();
            int p = progress.getPercent();
            if (p <= 0)
                continue;
            total += p;
        }
        return (int)((double)total / (double)jobCount);
    }

    private List<Integer> getIndices(Condition where) {
        String sql = "SELECT js.rowid AS rowid FROM jobs AS js "
            + "LEFT JOIN enum AS e "
            + "ON js.enum_id = e.rowid ";

        if (where != null)
            sql += "WHERE " + where.toString();

        try (PreparedStatement stmt = getConnection().prepareStatement(sql);
             ResultSet result = stmt.executeQuery()) {
            List<Integer> retval = new ArrayList<>();
            while (result.next())
                retval.add(result.getInt("rowid"));

            return Collections.unmodifiableList(retval);
        } catch (SQLException ex) {
            printError(ex.getErrorCode(), ex.getMessage());
            return Collections.unmodifiableList(new ArrayList());
        } catch (IOException ex) {
            printError(ex.getMessage());
            return Collections.unmodifiableList(new ArrayList());
        }
    }

    public List<Integer> getIndices() {
        return getIndices(null);
    }

    public Map<String, Number> getCombination(int jobId) {
        return getCombinationList().get(jobId);
    }

    public List<Map<String, Number>> getCombinationList() {
        int count = getCount();
        if (mCombinations != null && mCombinations.size() == count+1)
            return mCombinations;

        String sql = "SELECT js.rowid AS rowid, e.* FROM jobs AS js "
            + "LEFT JOIN enum AS e "
            + "ON js.enum_id = e.rowid ";

        try (Statement stmt = getConnection().createStatement();
             ResultSet result = stmt.executeQuery(sql)) {
            List<Map<String, Number>> retval =
                    new ArrayList<>();

             // SQLite indexing is based 1.
            retval.add(0, new HashMap<String, Number>());

            ResultSetMetaData metaData = result.getMetaData();
            int columnCount = metaData.getColumnCount();
            while (result.next()) {
                int id = -1;
                Map<String, Number> data = new HashMap<>();
                for (int index=1; index<=columnCount; index++) {
                    String column = metaData.getColumnName(index);
                    if ("rowid".equals(column)) {
                        id = result.getInt(column);
                    } else { // parameter name
                        data.put(column, result.getDouble(column));
                    }
                }
                if (id > 0 && data.size() > 0)
                    retval.add(id, data);
            }

            mCombinations = retval;
            return retval;
        } catch (SQLException ex) {
            printError(ex.getErrorCode(), ex.getMessage());
            return null;
        } catch (IOException ex) {
            printError(ex.getMessage());
            return null;
        }
    }
}
