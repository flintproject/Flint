/* -*- Mode: Java; tab-width: 4; indent-tabs-mode: nil; c-basic-offset: 4 -*- vim:set ts=4 sw=4 sts=4 et: */

package jp.oist.flint.dao;

import java.io.File;
import java.io.FileInputStream;
import java.io.IOException;
import java.io.RandomAccessFile;
import java.util.Collections;
import java.util.Map;

public class JobDao extends DaoObject {

    private final String mDirPath;

    private final static int CANCEL_OPEARTION = 0x01;

    private final Map<String, Number> mCombination;

    private final int mJobId;

    private final TaskDao mParent;

    public JobDao (TaskDao parent, String workingDir, Map<String, Number> combination, int jobId) {
        super();

        mParent = parent;
        mJobId = jobId;

        mCombination = combination;
        mDirPath = workingDir;
    }

    public TaskDao getParentTask () {
        return mParent;
    }

    public void cancel (boolean force) throws IOException {
            File controlFile = getControlFile();
            File jobDir = controlFile.getParentFile();

            if (!jobDir.exists())
                return;

            if (force) {
                if (!controlFile.exists()) {
                    controlFile.createNewFile();
                    getPercentageFile().createNewFile();
                }
            }

            if (!controlFile.exists() || !controlFile.canWrite())
                throw new IOException(String.format("%s didn't exist or you couldn't write into it.",
                        controlFile.getName()));

            if (getProgress() == 100)
                throw new IOException("It was already simulated.");

        try (RandomAccessFile raf = new RandomAccessFile(controlFile, "rws")) {
            raf.write(CANCEL_OPEARTION);
        }
    }

    public Map<String, Number> getCombination () {
        return Collections.unmodifiableMap(mCombination);
    }

    public int getProgress () {
            File percentageFile = getPercentageFile();
            if (!percentageFile.exists() || !percentageFile.canRead())
                return -1;
        try (FileInputStream fis = new FileInputStream(percentageFile)) {
            return fis.read();
        } catch (IOException ex) {
            return -1;
        }
    }

    public int getJobId () {
        return mJobId;
    }

    public File getControlFile () {
        return new File(mDirPath, "control");
    }

    public File getPercentageFile () {
        return new File(mDirPath, "status");
    }

    public File getIsdFile () {
        return new File(mDirPath, "isd");
    }

    public boolean isCancelled() {
            File controlFile = getControlFile();

            if (!controlFile.exists() || !controlFile.canWrite())
                return false;

            if (getProgress() == 100)
                return false;

        try (RandomAccessFile raf = new RandomAccessFile(controlFile, "rws")) {
            int operation = raf.read();
            return CANCEL_OPEARTION == operation;
        } catch (IOException ex) {
            return false;
        }
    }

    public boolean isCompleted() {
        return getProgress() == 100;
    }
}
