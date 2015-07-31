/* -*- Mode: Java; tab-width: 4; indent-tabs-mode: nil; c-basic-offset: 4 -*- vim:set ts=4 sw=4 sts=4 et: */

package jp.oist.flint.executor;

import jp.oist.flint.dao.JobDao;
import jp.oist.flint.dao.SimulationDao;
import jp.oist.flint.dao.TaskDao;
import jp.oist.flint.job.Progress;
import jp.oist.flint.sedml.ISimulationConfiguration;
import jp.oist.flint.sedml.ISimulationConfigurationList;
import org.apache.log4j.Logger;
import java.beans.PropertyChangeEvent;
import java.beans.PropertyChangeListener;
import java.beans.PropertyChangeSupport;
import java.io.IOException;
import java.util.HashMap;
import java.util.LinkedList;
import java.util.Map;
import org.apache.commons.vfs2.FileChangeEvent;
import org.apache.commons.vfs2.FileListener;
import org.apache.commons.vfs2.FileObject;
import org.apache.commons.vfs2.FileSystemException;
import org.apache.commons.vfs2.FileSystemManager;
import org.apache.commons.vfs2.VFS;
import org.apache.commons.vfs2.impl.DefaultFileMonitor;


public class PhspProgressMonitor implements FileListener, Runnable {

    private final DefaultFileMonitor mFileMonitor;

    private final PhspSimulator mSimulator;

    private final PropertyChangeSupport mPropetyChangeSupport;

    private final Thread mObserver;

    private final Object mLock = new Object();

    private volatile boolean mDone = false;

    private final LinkedList<JobDao> mQueue = new LinkedList<>();

    public PhspProgressMonitor (PhspSimulator simulator)
            throws FileSystemException {
        mObserver = new Thread(this);
        mSimulator = simulator;
        mPropetyChangeSupport = new PropertyChangeSupport(this);

        FileSystemManager mgr = VFS.getManager();
        mFileMonitor = new DefaultFileMonitor(this) {
            @Override
            protected void queueAddFile (FileObject fileObj) {
                if (isTaskDirecotry(fileObj) || isJobDirecotry(fileObj)
                        || "status".equals(fileObj.getName().getBaseName())
                        || "control".equals(fileObj.getName().getBaseName()))
                    super.queueAddFile(fileObj);
            }
        };
        mFileMonitor.setDelay(50);
        mFileMonitor.addFile(mgr.toFileObject(simulator.getWorkingDirectory()));
    }

    public void start () {
        mFileMonitor.start();
        mObserver.start();
    }

    public void stop () {
        mFileMonitor.stop();
        mDone = true;
        try {
            mObserver.join();
        } catch (InterruptedException ex) {
            Logger.getRootLogger().error(ex.getMessage());
            //ignored.
        }
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

        SimulationDao simulationDao = mSimulator.getSimulationDao();
        JobDao job = simulationDao.obtainJob(taskId, jobId);

        ISimulationConfiguration config = mSimulator.getSimulationConfigurationList().getConfiguration(taskId-1);
        Map<String, Number> combination = job.getCombination();
        String modelPath = config.getModelCanonicalPath();

        final PhspProgressMonitor.Event evt = new PhspProgressMonitor.Event(
        PhspProgressMonitor.this, "progress", null, progress);

        evt.setClientProperty("modelPath", modelPath);
        evt.setClientProperty("target", combination);

        getPropertyChangeSupport().firePropertyChange(evt);
    }

    @Override
    public final void fileCreated(FileChangeEvent evt) throws IOException {
        FileObject fileObj = evt.getFile();
        if (this.isJobDirecotry(fileObj)) {
                String baseName = fileObj.getParent().getName().getBaseName();
                int taskId = Integer.parseInt(baseName);

                baseName = fileObj.getName().getBaseName();
                int jobId = Integer.parseInt(baseName);

                if (mSimulator == null || mSimulator.getSimulationDao() == null)
                    return;
                TaskDao task = mSimulator.getSimulationDao().obtainTask(taskId);
                JobDao job = task.obtainJob(jobId);

                if (task.isCancelled())
                    job.cancel();

                setProgress(taskId, jobId, job.getProgress());

                synchronized (mLock) {
                    mQueue.addFirst(job);
                }
        } else if ("status".equals(fileObj.getName().getBaseName())) {
            // nothing to do...

        }
    }

    @Override
    public final void fileDeleted(FileChangeEvent evt) {
        // nothing to do...
    }

    @Override 
    public final void fileChanged(FileChangeEvent evt) {
        // nothing to do...
    }

    @Override
    public void run() {
        while (!mDone) {
            synchronized (mLock) {
                for (int i=0; i<mQueue.size(); i++) {
                    JobDao job = mQueue.get(i);

                    Progress progress = job.getProgress();
                    int taskId = job.getParentTask().getTaskId();
                    int jobId  = job.getJobId();
                    setProgress(taskId, jobId, progress);

                    if (progress.isCompleted() || job.isCancelled())
                        mQueue.remove(i);
                }
            }

            try {
                Thread.sleep(100);
            } catch (InterruptedException ex) {
            }
        }

        int taskCount = mSimulator.getSimulationDao().getCount();
        for (int i=1; i<=taskCount; i++) {
            TaskDao task = mSimulator.getSimulationDao().obtainTask(i);
            int jobCount = task.getCount();
            for (int j=1; j<=jobCount; j++) {
                JobDao job = task.obtainJob(j);
                setProgress(i, j, job.getProgress());
            }
        }
    }

    public boolean isTaskDirecotry (FileObject fileObj) {
        try {
            return isNaturalNumber(fileObj.getName().getBaseName())
                    && !isNaturalNumber(fileObj.getParent().getName().getBaseName());
        } catch (FileSystemException ex) {
            return false;
        }
    }

    public boolean isJobDirecotry (FileObject fileObj) {
        try {
            return isNaturalNumber(fileObj.getName().getBaseName())
                    && isNaturalNumber(fileObj.getParent().getName().getBaseName());
        } catch (FileSystemException ex) {
            return false;
        }
    }

    private boolean isNaturalNumber (String s) {
        try {
            long l = Long.parseLong(s);

            return isNaturalNumber(l);
        } catch (NumberFormatException ex) {
            return false;
        }
    }

    private boolean isNaturalNumber (Long l) {
        return l > 0;
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
