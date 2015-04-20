/* -*- Mode: Java; tab-width: 4; indent-tabs-mode: nil; c-basic-offset: 4 -*- vim:set ts=4 sw=4 sts=4 et: */
package jp.oist.flint.dao;

import org.apache.log4j.Logger;
import org.sqlite.SQLiteConfig;
import java.io.File;
import java.io.IOException;
import java.sql.Connection;
import java.sql.DriverManager;
import java.sql.SQLException;
import java.util.ArrayList;
import java.util.List;

public abstract class DaoObject implements AutoCloseable {

    static {
        try {
            Class.forName("org.sqlite.JDBC");
        } catch (ClassNotFoundException ex) {
            // ignored.
        }
    }

    protected Connection mConnection;

    /**
     * The working directory containing the database file.
     */
    protected final File mWorkingDir;

    private String mDbName;

    public DaoObject(String dbName, File workingDir)
            throws SQLException {
        if (dbName == null || dbName.isEmpty())
            throw new IllegalArgumentException("dbName must be non null.");

        File dbFile = new File(workingDir, dbName);

        mWorkingDir = workingDir;
        mDbName = dbName;
        mConnection = null;
    }

    public void connect () throws SQLException, IOException {
        if (mWorkingDir == null || mDbName == null)
            throw new IOException("It's don't  initialize yet.");
        String driverName;
        String dsn;
        File dbFile = new File(mWorkingDir, mDbName);
            dsn = String.format("jdbc:sqlite:%s", dbFile.getPath());

        if (mConnection != null)
            mConnection.close();

        SQLiteConfig config = new SQLiteConfig();
        config.setReadOnly(true);
        mConnection = DriverManager.getConnection(dsn, config.toProperties());
    }

    public File getDatabaseFile () {
        return new File(mWorkingDir, mDbName);
    }

    public String getDatabaseName () {
        return mDbName;
    }

    Connection getConnection () 
            throws SQLException, IOException {
        if (mConnection == null)
            connect();
        return mConnection;
    }

    @Override
    public void close () throws SQLException {
        if (mConnection != null)
            mConnection.close();

    }

    void printError (int errCode, String msg) {
        Logger.getRootLogger().error(String.format("ERROR [%s] : %s", 
                errCode, msg));
    }

    void printError (String msg) {
        Logger.getRootLogger().error(String.format("ERROR : %s", msg));
    }

    public static class Condition {

        private Condition mParent = null;

        private final String mLogic;

        private final String mSentence;

        private final List<Condition> mConditions ;

        public Condition () {
            this("", "");
        }

        public Condition (String sentence) {
            this("", sentence);
        }

        public Condition (String logic, String sentence) {
            mLogic = logic;
            mSentence = sentence;
            mConditions = new ArrayList<>();
        }

        public void addCondition (Condition condition) {
            condition.mParent = this;
            mConditions.add(condition);
        }

        public void removeCondition (Condition condition) {
            mConditions.remove(condition);
        }

        @Override
        public String toString () {
            StringBuilder sb = new StringBuilder();

            if (mSentence != null && !mSentence.isEmpty())
                sb.append(mSentence).append(" ");

            if (mLogic != null && !mLogic.isEmpty())
                sb.append(mLogic).append(" ");

            StringBuilder condition = new StringBuilder();
            for (Condition cond : mConditions)
                condition.append(cond.toString());

            String retval;
            if (condition.length() > 0) {
                int lastIndex = mConditions.size() - 1;
                Condition last = mConditions.get(lastIndex);
                String tmp = condition.substring(0, 
                        condition.length() - (last.mLogic.length()+2));

                sb.append("(").append(tmp).append(")");

                retval = "(" + sb.toString() + ")";

            } else {
                retval = sb.toString();
            }

            return retval;
        }
    }
}
