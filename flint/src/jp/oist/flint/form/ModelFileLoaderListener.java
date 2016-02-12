/* -*- Mode: Java; tab-width: 4; indent-tabs-mode: nil; c-basic-offset: 4 -*- vim:set ts=4 sw=4 sts=4 et: */
package jp.oist.flint.form;

import jp.oist.flint.backend.ModelLoader;
import jp.oist.flint.desktop.Desktop;
import jp.oist.flint.desktop.Document;
import java.beans.PropertyChangeEvent;
import java.beans.PropertyChangeListener;
import java.io.IOException;
import java.util.concurrent.ExecutionException;
import javax.swing.SwingWorker;

public class ModelFileLoaderListener implements PropertyChangeListener {

    private final ModelLoaderLogger mLogger;
    private final ModelLoader mLoader;

    public ModelFileLoaderListener(ModelLoaderLogger logger, ModelLoader loader) {
        mLogger = logger;
        mLoader = loader;
    }

    @Override
    public void propertyChange(PropertyChangeEvent evt) {
        String propertyName = evt.getPropertyName();
        Object nv = evt.getNewValue();
        if ("state".equals(propertyName)
            && nv == SwingWorker.StateValue.DONE) {
            Document document;
            try {
                document = mLoader.get();
            } catch (ExecutionException | InterruptedException e) {
                Throwable cause = e.getCause();
                String message = cause == null ? e.getMessage() : cause.getMessage();
                mLogger.showErrorDialog(message, "Error on opening model");
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
