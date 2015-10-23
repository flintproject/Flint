/* -*- Mode: Java; tab-width: 4; indent-tabs-mode: nil; c-basic-offset: 4 -*- vim:set ts=4 sw=4 sts=4 et: */
package jp.oist.flint.form.job;

import jp.oist.flint.job.Job;

public interface IProgressManager {
    void setProgress(int index, Job job);

    void setCancelled (int i, boolean cancelled);

    int indexOf (Object key);
}
