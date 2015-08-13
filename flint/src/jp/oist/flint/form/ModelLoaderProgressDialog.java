/* -*- Mode: Java; tab-width: 4; indent-tabs-mode: nil; c-basic-offset: 4 -*- vim:set ts=4 sw=4 sts=4 et: */
package jp.oist.flint.form;

import java.beans.PropertyChangeEvent;
import java.beans.PropertyChangeListener;
import javax.swing.JDialog;
import javax.swing.JProgressBar;
import javax.swing.SwingWorker;

class ModelLoaderProgressDialog implements PropertyChangeListener {

    private final JDialog mDialog;

    public ModelLoaderProgressDialog(MainFrame frame, String path) {
        JProgressBar bar = new JProgressBar();
        bar.setIndeterminate(true);
        bar.setString(path);
        bar.setStringPainted(true);
        mDialog = new JDialog(frame, "Loading a model", true);
        mDialog.add(bar);
        mDialog.pack();
        mDialog.setLocationRelativeTo(frame);
    }

    @Override
    public void propertyChange(PropertyChangeEvent evt) {
        String propertyName = evt.getPropertyName();
        Object nv = evt.getNewValue();
        if ("state".equals(propertyName)) {
            if (SwingWorker.StateValue.STARTED.equals(nv)) {
                mDialog.setVisible(true);
            } else if (SwingWorker.StateValue.DONE.equals(nv)) {
                mDialog.dispose();
            }
        }
    }
}
