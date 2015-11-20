/* -*- Mode: Java; tab-width: 4; indent-tabs-mode: nil; c-basic-offset: 4 -*- vim:set ts=4 sw=4 sts=4 et: */
package jp.oist.flint.executor;

import jp.oist.flint.desktop.Desktop;
import jp.oist.flint.desktop.Document;
import jp.oist.flint.form.sub.SubFrame;
import java.util.concurrent.Callable;
import java.util.concurrent.ExecutionException;
import java.util.concurrent.FutureTask;
import javax.swing.JOptionPane;
import javax.swing.SwingUtilities;
import javax.swing.SwingWorker;

public class ModelReloader extends SwingWorker<Boolean, Void> {

    private final Desktop mDesktop;
    private final SubFrame mTargetFrame;
    private final Document mDocument;

    public ModelReloader(Desktop desktop, SubFrame frame) {
        mDesktop = desktop;
        mTargetFrame = frame;
        mDocument = frame.getDocument();
    }

    @Override
    protected Boolean doInBackground() throws ExecutionException, InterruptedException {
        if (!mTargetFrame.tryLock()) return Boolean.FALSE;
        try {
            final Callable<Boolean> callable = new Callable<Boolean>() {
                @Override
                public Boolean call() {
                    int a = JOptionPane.showConfirmDialog(mDesktop.getPane(),
                                                          "The model file " + mDocument.getFile() + " seems changed.\n" + "Would you like to reload the model file?",
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
        if (r) {
            mDesktop.removeDocument(mDocument);
            mDesktop.openFile(mDocument.getFile());
        }
    }
}
