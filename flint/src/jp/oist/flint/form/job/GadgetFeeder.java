/* -*- Mode: Java; tab-width: 4; indent-tabs-mode: nil; c-basic-offset: 4 -*- vim:set ts=4 sw=4 sts=4 et: */
package jp.oist.flint.form.job;

import jp.oist.flint.export.ExportWorker;
import jp.oist.flint.filesystem.Workspace;
import jp.oist.flint.form.IFrame;
import jp.oist.flint.form.sub.JobWindow;
import jp.oist.flint.garuda.GarudaClient;
import jp.sbi.garuda.platform.commons.Gadget;
import jp.sbi.garuda.platform.commons.exception.NetworkException;
import java.io.File;
import java.io.IOException;
import java.util.concurrent.ExecutionException;
import javax.swing.SwingWorker;

public class GadgetFeeder extends SwingWorker<File, Boolean> {

    final JobWindow mWindow;
    final IFrame mFrame;
    final Gadget mGadget;
    final File mFile;
    final String mExtension;

    public GadgetFeeder(JobWindow window,
                        IFrame frame,
                        Gadget gadget,
                        File file,
                        String extension) {
        mWindow = window;
        mFrame = frame;
        mGadget = gadget;
        mFile = file;
        mExtension = extension;
    }

    @Override
    protected File doInBackground()
        throws ExecutionException, IOException, InterruptedException {
        if ("isd".equalsIgnoreCase(mExtension)) {
            return mFile;
        }
        // csv
        File csvFile = Workspace.createTempFile("export", ".csv");
        csvFile.deleteOnExit();
        ExportWorker worker = new ExportWorker(mFrame, mFile, csvFile);
        mWindow.addPropertyChangeListenerForProgress(worker.getMonitor());
        worker.execute();
        return worker.get() ? csvFile : null;
    }

    @Override
    protected void done() {
        try {
            File file = get();
            if (file == null) {
                // TODO
                return;
            }
            GarudaClient.sentFileToGadget(mGadget, file);
        } catch (ExecutionException | InterruptedException e) {
            mFrame.showErrorDialog(e.getMessage(), "Failed to send via Garuda");
        } catch (IllegalStateException | NetworkException ne) {
            mFrame.showErrorDialog(ne.getMessage(), "Error with Garuda");
        }
    }
}
