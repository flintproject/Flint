/* -*- Mode: Java; tab-width: 4; indent-tabs-mode: nil; c-basic-offset: 4 -*- vim:set ts=4 sw=4 sts=4 et: */
package jp.oist.flint.form;

import jp.oist.flint.backend.ModelLoader;
import jp.oist.flint.form.sub.SubFrame;
import jp.physiome.Ipc;
import java.beans.PropertyChangeEvent;
import java.beans.PropertyChangeListener;
import java.io.File;
import java.io.IOException;
import java.util.concurrent.ExecutionException;
import javax.swing.SwingWorker;

class ModelFileLoaderListener implements PropertyChangeListener {

    private final ModelLoaderLogger mLogger;
    private final File mFile;
    private final ModelLoader mLoader;

    public ModelFileLoaderListener(ModelLoaderLogger logger, File file, ModelLoader loader) {
        mLogger = logger;
        mFile = file;
        mLoader = loader;
    }

    @Override
    public void propertyChange(PropertyChangeEvent evt) {
        String propertyName = evt.getPropertyName();
        Object nv = evt.getNewValue();
        if ("state".equals(propertyName)
            && nv == SwingWorker.StateValue.DONE) {
            Ipc.ModelProbeResponse response;
            MainFrame mainJFrame = mLogger.getFrame();
            try {
                response = mLoader.get();
            } catch (ExecutionException | InterruptedException e) {
                mLogger.showErrorDialog(e.getMessage(), "Error on opening model");
                return;
            }
            if (response.getStatus() != Ipc.ModelProbeResponse.Status.OK) {
                mLogger.showErrorDialog(response.getErrorMessage(), "Error on opening model");
                return;
            }
            try {
                SubFrame frame = new SubFrame(mainJFrame, mFile, mLoader);
                mainJFrame.notifySubJFrameAdded(frame);
            } catch (ExecutionException | IOException | InterruptedException e) {
                mLogger.showErrorDialog(e.getMessage(), "Error on opening model");
            }
        }
    }
}
