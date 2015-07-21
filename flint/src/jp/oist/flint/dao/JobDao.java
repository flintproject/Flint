/* -*- Mode: Java; tab-width: 4; indent-tabs-mode: nil; c-basic-offset: 4 -*- vim:set ts=4 sw=4 sts=4 et: */
package jp.oist.flint.dao;

import jp.oist.flint.job.Progress;
import java.io.File;
import java.io.FileInputStream;
import java.io.IOException;
import java.io.RandomAccessFile;
import java.util.Collections;
import java.util.Map;

public class JobDao {

    private final File mWorkingDir;

    private final File mStatusFile;

    private final static int CANCEL_OPEARTION = 0x01;

    private final Map<String, Number> mCombination;

    private final int mJobId;

    private final TaskDao mParent;

    public JobDao(TaskDao parent, File workingDir, Map<String, Number> combination, int jobId) {
        mParent = parent;
        mWorkingDir = workingDir;
        mStatusFile = new File(mWorkingDir, "status");
        mJobId = jobId;

        mCombination = combination;
    }

    public TaskDao getParentTask() {
        return mParent;
    }

    public void cancel(boolean force) throws IOException {
        File controlFile = getControlFile();
        File jobDir = controlFile.getParentFile();

        if (!jobDir.exists())
            return;

        if (force) {
            if (!controlFile.exists()) {
                controlFile.createNewFile();
                mStatusFile.createNewFile();
            }
        }

        if (!controlFile.exists() || !controlFile.canWrite())
            throw new IOException(String.format("%s didn't exist or you couldn't write into it.",
                                                controlFile.getName()));

        if (getProgress().isCompleted())
            throw new IOException("It was already simulated.");

        try (RandomAccessFile raf = new RandomAccessFile(controlFile, "rws")) {
            raf.write(CANCEL_OPEARTION);
        }
    }

    public Map<String, Number> getCombination() {
        return Collections.unmodifiableMap(mCombination);
    }

    public Progress getProgress() {
        return new Progress(new File(mWorkingDir, "start"), mStatusFile);
    }

    public int getJobId() {
        return mJobId;
    }

    public File getControlFile() {
        return new File(mWorkingDir, "control");
    }

    public File getIsdFile() {
        return new File(mWorkingDir, "isd");
    }

    public boolean isCancelled() {
        File controlFile = getControlFile();

        if (!controlFile.exists() || !controlFile.canWrite())
            return false;

        if (getProgress().isCompleted())
            return false;

        try (RandomAccessFile raf = new RandomAccessFile(controlFile, "rws")) {
            int operation = raf.read();
            return CANCEL_OPEARTION == operation;
        } catch (IOException ex) {
            return false;
        }
    }

    public boolean isFinished() {
        Progress progress = getProgress();
        return progress.isCompleted() || isCancelled();
    }
}
