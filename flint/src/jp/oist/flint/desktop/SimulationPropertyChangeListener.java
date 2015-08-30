/* -*- Mode: Java; tab-width: 4; indent-tabs-mode: nil; c-basic-offset: 4 -*- vim:set ts=4 sw=4 sts=4 et: */
package jp.oist.flint.desktop;

import jp.oist.flint.executor.PhspSimulator;
import java.beans.PropertyChangeEvent;
import java.beans.PropertyChangeListener;
import javax.swing.SwingWorker;

class SimulationPropertyChangeListener implements PropertyChangeListener {

    private final ISimulationListener mListener;

    private boolean mDone;

    public SimulationPropertyChangeListener(ISimulationListener listener) {
        mListener = listener;
        mDone = false;
    }

    @Override
    public void propertyChange(PropertyChangeEvent evt) {
        String propertyName = evt.getPropertyName();
        Object nv = evt.getNewValue();
        if ("state".equals(propertyName)) {
            if (SwingWorker.StateValue.STARTED.equals(nv)) {
                if (!mDone)
                    mListener.simulationStarted((PhspSimulator)evt.getSource());
            } else if (SwingWorker.StateValue.DONE.equals(nv)) {
                mDone = true;
                mListener.simulationDone();
            }
        }
    }
}
