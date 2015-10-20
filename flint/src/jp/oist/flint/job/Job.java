/* -*- Mode: Java; tab-width: 4; indent-tabs-mode: nil; c-basic-offset: 4 -*- vim:set ts=4 sw=4 sts=4 et: */
package jp.oist.flint.job;

import java.io.File;
import java.io.IOException;
import java.io.RandomAccessFile;
import java.util.Collections;
import java.util.Locale;
import java.util.Map;

public class Job {

    private final File mWorkingDir;

    private final File mStatusFile;

    private final static int CANCEL_OPEARTION = 0x01;

    private final Map<String, Number> mCombination;

    private final int mJobId;

    private final int mTaskId;

    public Job(int taskId, File workingDir, Map<String, Number> combination, int jobId) {
        mTaskId = taskId;
        mWorkingDir = workingDir;
        mStatusFile = new File(mWorkingDir, "status");
        mJobId = jobId;

        mCombination = combination;
    }

    public int getTaskId() {
        return mTaskId;
    }

    /*
     * Return true if cancellation succeeded, otherwise false.
     */
    public boolean cancel() throws IOException {
        File controlFile = getControlFile();
        File jobDir = controlFile.getParentFile();
        if (!jobDir.exists())
            return false;
        if ( !controlFile.exists() &&
             !controlFile.createNewFile() )
            return false;
        if ( !mStatusFile.exists() &&
             !mStatusFile.createNewFile() )
            return false;
        if (getProgress().isCompleted())
            return false;
        try (RandomAccessFile raf = new RandomAccessFile(controlFile, "rws")) {
            raf.write(CANCEL_OPEARTION);
        }
        return true;
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

    public static File buildPath(File dir, int id) {
        assert id >= 0;

        int a = (id>>24)&0xFF;
        int b = (id>>16)&0xFF;
        int c = (id>> 8)&0xFF;
        int d = (id    )&0xFF;
        File dirA = new File(dir, String.format(Locale.ENGLISH, "%02x", a));
        File dirB = new File(dirA, String.format(Locale.ENGLISH, "%02x", b));
        File dirC = new File(dirB, String.format(Locale.ENGLISH, "%02x", c));
        return new File(dirC, String.format(Locale.ENGLISH, "%02x", d));
    }
}
