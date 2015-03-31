/* -*- Mode: Java; tab-width: 4; indent-tabs-mode: nil; c-basic-offset: 4 -*- vim:set ts=4 sw=4 sts=4 et: */
package jp.oist.flint.phsp;

import jp.oist.flint.backend.ModelLoader;
import jp.oist.flint.form.IMainFrame;
import jp.oist.flint.form.ModelLoaderLogger;
import jp.oist.flint.form.sub.SubFrame;
import jp.oist.flint.phsp.entity.Model;
import jp.physiome.Ipc;
import java.beans.PropertyChangeEvent;
import java.beans.PropertyChangeListener;
import java.io.File;
import java.io.IOException;
import java.util.concurrent.ExecutionException;
import javax.swing.SwingWorker;

public class PhspReaderListener implements PropertyChangeListener {

    private final ModelLoaderLogger mLogger;

    public PhspReaderListener(ModelLoaderLogger logger) {
        mLogger = logger;
    }

    @Override
    public void propertyChange(PropertyChangeEvent evt) {
        String propertyName = evt.getPropertyName();
        if (!"LOADED".equals(propertyName)) return;

        Model model = (Model)evt.getNewValue();

        File file = model.getModelFile();

        try {
            ModelLoader loader = new ModelLoader(file);
            loader.addPropertyChangeListener(new PhspModelFileLoaderListener(mLogger, model, loader));
            loader.execute();
        } catch (IOException ioe) {
            mLogger.showErrorDialog(ioe.getMessage(), "Error on opening model");
        }
    }

    private class PhspModelFileLoaderListener implements PropertyChangeListener {

        private final ModelLoaderLogger mLogger;
        private final Model mModel;
        private final ModelLoader mFileLoader;


        private PhspModelFileLoaderListener(ModelLoaderLogger logger, Model model, ModelLoader fileLoader) {
            mLogger = logger;
            mModel = model;
            mFileLoader = fileLoader;
        }

        @Override
        public void propertyChange(PropertyChangeEvent evt) {
            String propertyName = evt.getPropertyName();
            Object stateValue = evt.getNewValue();

            if ("state".equals(propertyName)
                    && SwingWorker.StateValue.DONE == stateValue) {

                Ipc.ModelProbeResponse response;
                IMainFrame mainJFrame = mLogger.getFrame();
                try {
                    response = mFileLoader.get();
                } catch (ExecutionException | InterruptedException ee) {
                    mLogger.showErrorDialog(ee.getMessage(), "Error on opening model");
                    return;
                }
                if (response.getStatus() != Ipc.ModelProbeResponse.Status.OK) {
                    mLogger.showErrorDialog(response.getErrorMessage(), "Error on opening model");
                    return;
                }
                try {
                    SubFrame frame = new SubFrame(mainJFrame, mModel, mFileLoader);
                    mainJFrame.notifySubJFrameAdded(frame);
                } catch (ExecutionException | IOException | InterruptedException ee) {
                    mLogger.showErrorDialog(ee.getMessage(), "Error on opening model");
                }
            }
        }
    }
}



