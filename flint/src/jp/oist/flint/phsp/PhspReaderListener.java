/* -*- Mode: Java; tab-width: 4; indent-tabs-mode: nil; c-basic-offset: 4 -*- vim:set ts=4 sw=4 sts=4 et: */
package jp.oist.flint.phsp;

import jp.oist.flint.backend.ModelLoader;
import jp.oist.flint.desktop.Desktop;
import jp.oist.flint.desktop.Document;
import jp.oist.flint.form.ModelLoaderLogger;
import jp.oist.flint.form.sub.SubFrame;
import jp.oist.flint.phsp.entity.Model;
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
        ModelLoader loader = new ModelLoader(file);
        loader.addPropertyChangeListener(new PhspModelFileLoaderListener(mLogger, loader));
        loader.execute();
    }

    private static class PhspModelFileLoaderListener implements PropertyChangeListener {

        private final ModelLoaderLogger mLogger;
        private final ModelLoader mFileLoader;

        private PhspModelFileLoaderListener(ModelLoaderLogger logger, ModelLoader fileLoader) {
            mLogger = logger;
            mFileLoader = fileLoader;
        }

        @Override
        public void propertyChange(PropertyChangeEvent evt) {
            String propertyName = evt.getPropertyName();
            Object stateValue = evt.getNewValue();

            if ("state".equals(propertyName)
                    && SwingWorker.StateValue.DONE == stateValue) {
                Document document;
                try {
                    document = mFileLoader.get();
                } catch (ExecutionException | InterruptedException ee) {
                    mLogger.showErrorDialog(ee.getMessage(), "Error on opening model");
                    return;
                }
                Desktop desktop = mLogger.getDesktop();
                try {
                    desktop.addDocument(document);
                } catch (IOException ioe) {
                    mLogger.showErrorDialog(ioe.getMessage(), "Error on opening model");
                }
            }
        }
    }
}
