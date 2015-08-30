/* -*- Mode: Java; tab-width: 4; indent-tabs-mode: nil; c-basic-offset: 4 -*- vim:set ts=4 sw=4 sts=4 et: */

package jp.oist.flint.executor;

import jp.oist.flint.phsp.PhspException;
import jp.oist.flint.sedml.SedmlException;
import java.io.File;
import java.io.FileOutputStream;
import java.io.IOException;
import java.sql.SQLException;
import javax.swing.SwingWorker;
import javax.xml.parsers.ParserConfigurationException;
import javax.xml.transform.TransformerException;
import jp.oist.flint.dao.SimulationDao;
import jp.oist.flint.filesystem.Workspace;
import jp.oist.flint.phsp.IPhspConfiguration;
import jp.oist.flint.phsp.PhspWriter;
import jp.oist.flint.phsp.entity.Model;
import jp.oist.flint.sedml.ISimulationConfigurationList;
import jp.oist.flint.sedml.SedmlWriter;

public class PhspSimulator extends SwingWorker <Boolean, Integer> {

    private final SimulatorService mService;

    private final File mWorkingDir;

    private SimulationDao mSimulationDao;

    private final ISimulationConfigurationList mSedmlConfig;

    private IPhspConfiguration mPhspConfig = null;

    private File mSedmlFile = null;

    private File mPhspFile = null;

    private File mLogFile = null;

    private FlintExecJob mFlintExecJob = null;

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

    public boolean isStarted () {
        return StateValue.STARTED.equals(getState());
    }

    @Override
    protected Boolean doInBackground() throws Exception {
        File sedmlFile = mSedmlFile;
        File phspFile = mPhspFile;

        mSimulationDao = new SimulationDao(mWorkingDir);
        mLogFile = new File(mWorkingDir, "flint.log");
        mLogFile.deleteOnExit();

        mFlintExecJob = new FlintExecJob(sedmlFile, phspFile, mWorkingDir);
        return mService.call(mFlintExecJob, mLogFile);
    }

}
