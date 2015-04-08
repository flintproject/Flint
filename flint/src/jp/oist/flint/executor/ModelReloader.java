/* -*- Mode: Java; tab-width: 4; indent-tabs-mode: nil; c-basic-offset: 4 -*- vim:set ts=4 sw=4 sts=4 et: */
package jp.oist.flint.executor;

import jp.oist.flint.form.MainFrame;
import java.io.File;
import java.util.concurrent.Callable;
import java.util.concurrent.ExecutionException;
import java.util.concurrent.FutureTask;
import javax.swing.JOptionPane;
import javax.swing.SwingUtilities;
import javax.swing.SwingWorker;
import jp.oist.flint.form.sub.SubFrame;

public class ModelReloader extends SwingWorker<Boolean, Void> {

    private final MainFrame mParentFrame;
    private final SubFrame mTargetFrame;
    private final File mModelFile;

    public ModelReloader(MainFrame parentFrame, SubFrame targetFrame) {
        mParentFrame = parentFrame;
        mTargetFrame = targetFrame;
        mModelFile = targetFrame.getModelFile();
    }

    @Override
    protected Boolean doInBackground() throws ExecutionException, InterruptedException {
        if (!mTargetFrame.tryLock()) return Boolean.FALSE;
        try {
            final Callable<Boolean> callable = new Callable<Boolean>() {
                @Override
                public Boolean call() {
                    int a = JOptionPane.showConfirmDialog((java.awt.Container)mTargetFrame,
                                                          "The model file " + mModelFile.getPath() + " seems changed.\n" + "Would you like to reload the model file?",
                                                          "Reload the model file?",
                                                          JOptionPane.YES_NO_OPTION);
                    return a == JOptionPane.YES_OPTION;
                }
            };
            final FutureTask<Boolean> task = new FutureTask<>(callable);
            SwingUtilities.invokeLater(task);
            return task.get();
        } finally {
            mTargetFrame.unlock();
        }
    }

    @Override
    protected void done() {
        Boolean r;
        try {
            r = get();
        } catch (ExecutionException | InterruptedException e) {
            // ignored
            return;
        }
        if (r.booleanValue()) {
            mParentFrame.closeModel(mTargetFrame);
            mParentFrame.openModel(mModelFile);
        }
    }
}
