/* -*- Mode: Java; tab-width: 4; indent-tabs-mode: nil; c-basic-offset: 4 -*- vim:set ts=4 sw=4 sts=4 et: */

package jp.oist.flint.executor;

import jp.oist.flint.phsp.PhspException;
import jp.oist.flint.sedml.SedmlException;
import java.io.File;
import java.io.FileOutputStream;
import java.io.IOException;
import java.text.SimpleDateFormat;
import java.util.Date;
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

    private final SimulationDao mSimulationDao;

    private final ISimulationConfigurationList mSedmlConfig;

    private final IPhspConfiguration mPhspConfig;

    private final File mSedmlFile;

    private final File mPhspFile;

    private final File mLogFile;

    public PhspSimulator (SimulatorService service, 
                            ISimulationConfigurationList sedml, 
                            IPhspConfiguration phsp) 
        throws IOException, ParserConfigurationException, PhspException,
               SedmlException, TransformerException {

        mService = service;
        mSedmlConfig = sedml;
        mPhspConfig = phsp;

        Model[] models = phsp.getModels();

        for (Model model : models)
            model.validate();

        SimpleDateFormat sdf = new SimpleDateFormat("yyyyMMddHHmmss-");
        String prefix = sdf.format(new Date());
        mWorkingDir = Workspace.createTempDirectory(prefix);

        mPhspFile = new File(mWorkingDir, "input.phsp");
        mPhspFile.deleteOnExit();

        mSedmlFile = new File(mWorkingDir, "input.xml");
        mSedmlFile.deleteOnExit();

        try (FileOutputStream phspStream = new FileOutputStream(mPhspFile)) {
            PhspWriter phspWriter = new PhspWriter();

            phspWriter.write(mPhspConfig, phspStream, true);
        }
        try (FileOutputStream sedmlStream = new FileOutputStream(mSedmlFile)) {
            SedmlWriter sedmlWriter = new SedmlWriter(true);
            sedmlWriter.writeSimulationConfiguration(mSedmlConfig, sedmlStream);
        }
        mSimulationDao = new SimulationDao(mWorkingDir);
        mLogFile = new File(mWorkingDir, "flint.log");
        mLogFile.deleteOnExit();
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
        FlintExecJob job = new FlintExecJob(mSedmlFile, mPhspFile, mWorkingDir);
        return mService.call(job, mLogFile);
    }

}
