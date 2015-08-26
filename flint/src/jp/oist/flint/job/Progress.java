/* -*- Mode: Java; tab-width: 4; indent-tabs-mode: nil; c-basic-offset: 4 -*- vim:set ts=4 sw=4 sts=4 et: */
package jp.oist.flint.job;

import java.io.File;
import java.io.FileInputStream;
import java.io.IOException;

public class Progress {

    private long mStarted;

    private long mLastUpdated;

    private final int mPercent;

    public Progress(File start, File file) {
        mStarted = start.lastModified();
        mLastUpdated = file.lastModified();
        if (mLastUpdated < mStarted) {
            mLastUpdated = mStarted;
        }
        if (!file.exists() || !file.canRead()) {
            mPercent = 0;
            return;
        }
        int p = 0;
        try (FileInputStream fis = new FileInputStream(file)) {
            p = fis.read();
        } catch (IOException ioe) {
            // ignored
        } finally {
            mPercent = p;
        }
    }

    public long getStarted() {
        return mStarted;
    }

    public long getLastUpdated() {
        return mLastUpdated;
    }

    public long getElapsedMillis() {
        return mLastUpdated - mStarted;
    }

    public int getPercent() {
        return mPercent;
    }

    public boolean isCompleted() {
        return mPercent == 100;
    }
}
