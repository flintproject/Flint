/* -*- Mode: Java; tab-width: 4; indent-tabs-mode: nil; c-basic-offset: 4 -*- vim:set ts=4 sw=4 sts=4 et: */
package jp.oist.flint.executor;

import jp.oist.flint.dao.SimulationDao;
import jp.oist.flint.dao.DaoException;
import jp.oist.flint.dao.TaskDao;
import jp.oist.flint.job.Job;
import jp.oist.flint.job.Progress;
import jp.oist.flint.sedml.ISimulationConfiguration;
import jp.oist.flint.sedml.ISimulationConfigurationList;
import java.beans.PropertyChangeEvent;
import java.beans.PropertyChangeListener;
import java.beans.PropertyChangeSupport;
import java.io.IOException;
import java.sql.SQLException;
import java.util.HashMap;
import java.util.Map;
import javax.swing.SwingUtilities;

public class PhspProgressMonitor implements Runnable {

    private final SimulationDao mSimulationDao;

    private final ISimulationConfigurationList mSimulationConfigurationList;

    private final PropertyChangeSupport mPropetyChangeSupport;

    private final Thread mObserver;

    private volatile boolean mDone = false;

    public PhspProgressMonitor(PhspSimulator simulator) {
        mSimulationDao = simulator.getSimulationDao();
        mSimulationConfigurationList = simulator.getSimulationConfigurationList();
        mObserver = new Thread(this);
        mPropetyChangeSupport = new PropertyChangeSupport(this);
    }

    public void start () {
        mObserver.start();
    }

    public void stop () {
        mDone = true;
    }

    public PropertyChangeSupport getPropertyChangeSupport () {
        return mPropetyChangeSupport;
    }

    public void addPropertyChangeListener (PropertyChangeListener l) {
        mPropetyChangeSupport.addPropertyChangeListener(l);
    }

    public void addPropertyChangeListener (String proeprtyName, PropertyChangeListener l) {
        mPropetyChangeSupport.addPropertyChangeListener(proeprtyName, l);
    }

    public void removePropertyChangeListener (PropertyChangeListener l) {
        mPropetyChangeSupport.removePropertyChangeListener(l);
    }

    public void removePropertyChangeListener (String proeprtyName, PropertyChangeListener l) {
        mPropetyChangeSupport.removePropertyChangeListener(proeprtyName, l);
    }

    protected void firePropertyChange (String propertyName, Object oldValue, Object newValue) {
        mPropetyChangeSupport.firePropertyChange(propertyName, oldValue, newValue);
    }

    private void setProgress(int taskId, int jobId, final Progress progress) {
        if (!getPropertyChangeSupport().hasListeners("progress"))
            return;

        try {
            Job job = mSimulationDao.obtainJob(taskId, jobId);

            ISimulationConfiguration config = mSimulationConfigurationList.getConfiguration(taskId-1);
            Map<String, Number> combination = job.getCombination();
            String modelPath = config.getModelCanonicalPath();

            final PhspProgressMonitor.Event evt = new PhspProgressMonitor.Event(
                                                                                PhspProgressMonitor.this, "progress", null, progress);

            evt.setClientProperty("modelPath", modelPath);
            evt.setClientProperty("target", combination);

            SwingUtilities.invokeLater(new Runnable() {
                @Override
                public void run() {
                    getPropertyChangeSupport().firePropertyChange(evt);
                }
            });
        } catch (DaoException | IOException | SQLException ex) {
            // give up
        }
    }

    @Override
    public void run() {
        while (!mDone) {
            try {
                Thread.sleep(100);
            } catch (InterruptedException ex) {
            }

            try {
                int taskCount = mSimulationDao.getCount();
                for (int i=1; i<=taskCount; i++) {
                    TaskDao task = mSimulationDao.obtainTask(i);
                    if (!task.isStarted())
                        continue;
                    int jobCount = task.getCount();
                    for (int j=1; j<=jobCount; j++) {
                        Job job = task.obtainJob(j);
                        setProgress(i, j, job.getProgress());
                    }
                }
            } catch (DaoException | IOException | SQLException ex) {
                // give up
            }
        }

    }

    public static class Event extends PropertyChangeEvent {

        private final Map<Object, Object> mClientProperty;

        public Event(Object source, String propertyName, Object oldValue, Object newValue) {
            super(source, propertyName, oldValue, newValue);

            mClientProperty = new HashMap<>();
        }

        public Object getClientProperty (Object key) {
            return mClientProperty.get(key);
        }

        public void setClientProperty(Object key, Object value) {
            mClientProperty.put(key, value);
        }
    }
}
