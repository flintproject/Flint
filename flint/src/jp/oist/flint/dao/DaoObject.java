/* -*- Mode: Java; tab-width: 4; indent-tabs-mode: nil; c-basic-offset: 4 -*- vim:set ts=4 sw=4 sts=4 et: */
package jp.oist.flint.dao;

import org.apache.log4j.Logger;
import org.sqlite.SQLiteConfig;
import java.io.File;
import java.io.IOException;
import java.sql.Connection;
import java.sql.DriverManager;
import java.sql.SQLException;

public abstract class DaoObject implements AutoCloseable {

    static {
        try {
            Class.forName("org.sqlite.JDBC");
        } catch (ClassNotFoundException ex) {
            // ignored.
        }
    }

    /**
     * The working directory containing the database file.
     */
    protected final File mWorkingDir;

    /**
     * The database file.
     */
    private final File mDatabaseFile;

    /**
     * The cached connection.
     */
    private Connection mConnection;

    public DaoObject(String dbName, File workingDir)
            throws SQLException {
        if (dbName == null || dbName.isEmpty())
            throw new IllegalArgumentException("dbName must be non null.");

        mWorkingDir = workingDir;
        mDatabaseFile = new File(workingDir, dbName);
        mConnection = null;
    }

    public void connect() throws SQLException, IOException {
        String dsn = String.format("jdbc:sqlite:%s", mDatabaseFile.getCanonicalPath());

        if (mConnection != null)
            mConnection.close();

        SQLiteConfig config = new SQLiteConfig();
        config.setReadOnly(true);
        mConnection = DriverManager.getConnection(dsn, config.toProperties());
    }

    public Connection getConnection()
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

}
