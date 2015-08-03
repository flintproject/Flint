/* -*- Mode: Java; tab-width: 4; indent-tabs-mode: nil; c-basic-offset: 4 -*- vim:set ts=4 sw=4 sts=4 et: */

package jp.oist.flint.executor;

import jp.oist.flint.phsp.PhspException;
import jp.oist.flint.sedml.SedmlException;
import org.apache.log4j.Logger;
import java.beans.PropertyChangeEvent;
import java.beans.PropertyChangeListener;
import java.io.File;
import java.io.FileOutputStream;
import java.io.IOException;
import java.sql.SQLException;
import java.util.EventListener;
import java.util.EventObject;
import java.util.concurrent.ExecutionException;
import java.util.concurrent.Future;
import javax.swing.SwingWorker;
import javax.swing.event.EventListenerList;
import javax.xml.parsers.ParserConfigurationException;
import javax.xml.transform.TransformerException;
import jp.oist.flint.dao.SimulationDao;
import jp.oist.flint.filesystem.Workspace;
import jp.oist.flint.phsp.IPhspConfiguration;
import jp.oist.flint.phsp.PhspWriter;
import jp.oist.flint.phsp.entity.Model;
import jp.oist.flint.sedml.ISimulationConfigurationList;
import jp.oist.flint.sedml.SedmlWriter;

public class PhspSimulator extends SwingWorker <Boolean, Integer> 
    implements PropertyChangeListener {

    private final SimulatorService mService;

    private final File mWorkingDir;

    private final EventListenerList mEventListenerList 
            = new EventListenerList();

    private SimulationDao mSimulationDao;

    private final ISimulationConfigurationList mSedmlConfig;

    private IPhspConfiguration mPhspConfig = null;

    private File mSedmlFile = null;

    private File mPhspFile = null;

    private File mLogFile = null;

    private FlintExecJob mFlintExecJob = null;

    private boolean isSuccess = false;

    public PhspSimulator (SimulatorService service, 
                            ISimulationConfigurationList sedml, 
                            IPhspConfiguration phsp) 
        throws IOException, ParserConfigurationException, PhspException,
               SQLException, SedmlException, TransformerException {

        mService = service;
        mSedmlConfig = sedml;
        mPhspConfig = phsp;

        Model[] models = phsp.getModels();

        for (Model model : models)
            model.validate();


        mPhspFile = Workspace.createTempFile("phsp", ".phsp");
        mPhspFile.deleteOnExit();

        mSedmlFile = Workspace.createTempFile("xml", ".xml");
        mSedmlFile.deleteOnExit();

        try (FileOutputStream phspStream = new FileOutputStream(mPhspFile)) {
            PhspWriter phspWriter = new PhspWriter();

            phspWriter.write(mPhspConfig, phspStream, true);
        }
        try (FileOutputStream sedmlStream = new FileOutputStream(mSedmlFile)) {
            SedmlWriter sedmlWriter = new SedmlWriter(true);
            sedmlWriter.writeSimulationConfiguration(mSedmlConfig, sedmlStream);
        }
            mWorkingDir = Workspace.createTempDirectory("flint-exec");
            addPropertyChangeListener(this);
    }

    public File getSedmlFile () {
        return mSedmlFile;
    }

    public File getPhspFile () {
        return mPhspFile;
    }

    public File getLogFile () {
        return mLogFile;
    }

    public ISimulationConfigurationList getSimulationConfigurationList () {
        return mSedmlConfig;
    }

    public IPhspConfiguration getPhspConfiguration () {
        return mPhspConfig;
    }

    public File getWorkingDirectory () {
        return mWorkingDir;
    }

    public SimulationDao getSimulationDao () {
        return mSimulationDao;
    }

    public boolean isSuccess () {
        return isSuccess;
    }

    public boolean isStarted () {
        return StateValue.STARTED.equals(getState());
    }

    public void addSimulationListener (PhspSimulator.Listener l) {
        mEventListenerList.add(PhspSimulator.Listener.class, l);
    }

    public void removeSimulationListener (PhspSimulator.Listener l) {
        mEventListenerList.remove(PhspSimulator.Listener.class, l);
    }

    protected void fireSimulationEvent (String eventName) {
        PhspSimulator.Listener[] listeners 
                = mEventListenerList.getListeners(PhspSimulator.Listener.class);

        PhspSimulator.Event evt = new PhspSimulator.Event(this);
        if ("SimulationStarted".equals(eventName)) {
            for (PhspSimulator.Listener l : listeners)
                l.onSimulationStarted(evt);

        } else if ("SimulationExited".equals(eventName)) {
            for (PhspSimulator.Listener l : listeners)
                l.onSimulationExited(evt);
        }
    }

    @Override
    protected void done () {
        try {
            isSuccess = get();
        } catch (InterruptedException | ExecutionException ex) {
            Logger.getRootLogger().error(ex.getMessage());
            isSuccess = false;
        }
    }

    @Override
    protected Boolean doInBackground()
        throws ExecutionException, IOException, InterruptedException, SQLException {
        File sedmlFile = mSedmlFile;
        File phspFile = mPhspFile;

        mSimulationDao = new SimulationDao(mWorkingDir);
        mLogFile = new File(mWorkingDir, "flint.log");
        mLogFile.deleteOnExit();

        mFlintExecJob = new FlintExecJob(sedmlFile, phspFile, mWorkingDir);
        Future<Boolean> submit = mService.submit(mFlintExecJob, mLogFile);

        return submit.get();
    }

    @Override
    public void propertyChange(PropertyChangeEvent evt) {
        String propertyName = evt.getPropertyName();
        Object newValue = evt.getNewValue();

        if ("state".equals(propertyName)) {
            if (SwingWorker.StateValue.STARTED.equals(newValue)) {
                fireSimulationEvent("SimulationStarted");
            } else if (SwingWorker.StateValue.DONE.equals(newValue)) {
                fireSimulationEvent("SimulationExited");
            }
        }
    }

    public static class Event extends EventObject {

        private ISimulationConfigurationList mSedmlConfig;

        private IPhspConfiguration mPhspConfig;

        public Event(Object source) {
            super(source);
        }
    }

    public static interface Listener extends EventListener {
        void onSimulationStarted (PhspSimulator.Event evt);

        void onSimulationExited (PhspSimulator.Event evt);
    }
}
