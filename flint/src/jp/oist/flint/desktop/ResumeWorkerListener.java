/* -*- Mode: Java; tab-width: 4; indent-tabs-mode: nil; c-basic-offset: 4 -*- vim:set ts=4 sw=4 sts=4 et: */
package jp.oist.flint.desktop;

import java.beans.PropertyChangeEvent;
import java.beans.PropertyChangeListener;
import java.util.concurrent.ExecutionException;
import javax.swing.SwingWorker;

class ResumeWorkerListener implements PropertyChangeListener {

    private final ResumeWorker mWorker;

    private final ISimulationListener mListener;

    public ResumeWorkerListener(ResumeWorker worker, ISimulationListener listener) {
        mWorker = worker;
        mListener = listener;
    }

    @Override
    public void propertyChange(PropertyChangeEvent pce) {
        String propertyName = pce.getPropertyName();
        Object nv = pce.getNewValue();
        if ("state".equals(propertyName)) {
            if (SwingWorker.StateValue.DONE.equals(nv)) {
                try {
                    if (mWorker.get()) {
                        mListener.simulationResumed();
                    }
                } catch (ExecutionException | InterruptedException e) {
                    // ignored
                }
            }
        }
    }
}
