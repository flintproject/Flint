/* -*- Mode: Java; tab-width: 4; indent-tabs-mode: nil; c-basic-offset: 4 -*- vim:set ts=4 sw=4 sts=4 et: */
package jp.oist.flint.desktop;

import jp.oist.flint.backend.ModelLoader;
import jp.oist.flint.filesystem.ModelFileWatcher;
import jp.oist.flint.form.ModelFileLoaderListener;
import jp.oist.flint.form.ModelLoaderLogger;
import jp.oist.flint.form.ModelLoaderProgressDialog;
import jp.oist.flint.form.sub.SubFrame;
import jp.oist.flint.phsp.IPhspConfiguration;
import jp.oist.flint.phsp.PhspException;
import jp.oist.flint.phsp.entity.Model;
import org.apache.log4j.Logger;
import java.beans.PropertyVetoException;
import java.io.File;
import java.io.IOException;
import java.util.ArrayList;
import javax.swing.JDesktopPane;
import javax.swing.JInternalFrame;
import javax.swing.JOptionPane;
import javax.swing.event.EventListenerList;

/**
 * A desktop holds multiple documents; each of them corresponds to a model
 * opened by Flint.
 */
public class Desktop implements IPhspConfiguration {

    private final ArrayList<Document> mDocuments;

    private final EventListenerList mListeners;

    private final EventListenerList mLoadingListeners;

    private final JDesktopPane mPane;

    private final ModelFileWatcher mModelFileWatcher;

    public Desktop() throws IOException {
        mDocuments = new ArrayList<>();
        mListeners = new EventListenerList();
        mLoadingListeners = new EventListenerList();
        mPane = new JDesktopPane();
        mModelFileWatcher = new ModelFileWatcher();
    }

    public JDesktopPane getPane() {
        return mPane;
    }

    public int getSize() {
        return mDocuments.size();
    }

    public boolean isEmpty() {
        return mDocuments.isEmpty();
    }

    private void showErrorDialog(String message, String title) {
        JOptionPane.showMessageDialog(mPane, message, title, JOptionPane.ERROR_MESSAGE);
    }

    public boolean openFile(final File file) {
        if (file == null) {
            // just ignore it
            return false;
        }
        if (!file.exists()) {
            String msg = String.format("The file named \"%s\" does not exist.", file.getPath());
            showErrorDialog(msg, "Error on opening model");
            return false;
        }

        // check if the file is opened.
        for (Document doc : mDocuments) {
            if (file.equals(doc.getFile())) {
                try {
                    doc.getSubFrame().setSelected(true);
                } catch (PropertyVetoException ex) {
                    // ignored
                }
                return true;
            }
        }

        String path;
        try {
            path = file.getCanonicalPath();
        } catch (IOException ex) {
            showErrorDialog("could not get canonical path : " + file.toString(),
                            "Error on opening model");
            return false;
        }

        if (!file.isFile()) {
            showErrorDialog("could not get canonical path : " + file.toString(),
                            "Error on opening model"); return false; }

        int len = (int)file.length();
        if (len == 0) {
            showErrorDialog("file has length 0 : " + path,
                            "Error on opening model");
            return false;
        }

        ModelLoaderLogger logger = new ModelLoaderLogger(this);
        ModelLoader loader = new ModelLoader(file);
        loader.addPropertyChangeListener(new ModelFileLoaderListener(logger, loader));
        loader.addPropertyChangeListener(new ModelLoaderProgressDialog(null, path));
        for (ILoadingListener listener : mLoadingListeners.getListeners(ILoadingListener.class)) {
            loader.addPropertyChangeListener(new LoadingPropertyChangeListener(listener));
        }
        loader.execute();
        return true;
    }

    public void addDocument(Document document) throws IOException {
        SubFrame subFrame = new SubFrame(this, document);
        subFrame.setDefaultCloseOperation(JInternalFrame.DO_NOTHING_ON_CLOSE);
        subFrame.addInternalFrameListener(new SubFrameAdapter(this));

        try {
            mModelFileWatcher.watch(document.getFile(), subFrame);
        } catch (IOException ioe) {
            Logger.getRootLogger().error(ioe.getMessage());
        }

        subFrame.setVisible(true);
        mPane.add(subFrame);
        try {
            subFrame.setSelected(true);
            subFrame.setMaximum(true);
        } catch (PropertyVetoException ex) {
            // ignored
        }

        document.setSubFrame(subFrame);
        mDocuments.add(document);
        // notify listeners
        for (IDesktopListener listener : mListeners.getListeners(IDesktopListener.class)) {
            listener.documentAdded(document);
        }
    }

    public void removeDocument(Document document) {
        mModelFileWatcher.unwatch(document.getFile());
        SubFrame subFrame = document.getSubFrame();
        subFrame.dispose();
        mDocuments.remove(document);
        // notify listeners
        for (IDesktopListener listener : mListeners.getListeners(IDesktopListener.class)) {
            listener.documentRemoved(document, isEmpty());
        }
    }

    public void addListener(IDesktopListener listener) {
        mListeners.add(IDesktopListener.class, listener);
    }

    public void addLoadingListener(ILoadingListener listener) {
        mLoadingListeners.add(ILoadingListener.class, listener);
    }

    public void startWatching() {
        Thread t = new Thread(mModelFileWatcher);
        t.start();
    }

    public ArrayList<SubFrame> getSubFrames() {
        ArrayList<SubFrame> list = new ArrayList<>();
        for (Document doc : mDocuments) {
            list.add(doc.getSubFrame());
        }
        return list;
    }

    /* IPhspConfiguration */

    @Override
    public Model[] getModels() throws PhspException {
        int size = mDocuments.size();
        Model[] models = new Model[size];
        for (int i=0;i<size;i++) {
            models[i] = mDocuments.get(i).getSubFrame().getModel();
        }
        return models;
    }
}
