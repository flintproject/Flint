/* -*- Mode: Java; tab-width: 4; indent-tabs-mode: nil; c-basic-offset: 4 -*- vim:set ts=4 sw=4 sts=4 et: */
package jp.oist.flint.headless;

import java.io.File;
import javax.swing.SwingWorker;
import jp.oist.flint.executor.FlintRunJob;
import jp.oist.flint.executor.SimulatorService;
import jp.oist.flint.filesystem.Workspace;
import jp.physiome.Cli.RunOption;

public class Simulator extends SwingWorker <Boolean, Void> {

    private final SimulatorService mService;
    private final RunOption mOption;
    private FlintRunJob mJob;

    public Simulator (SimulatorService service, RunOption option) {
        mService = service;
        mOption = option;
    }

    @Override
    protected Boolean doInBackground()
        throws Exception {
            File executionDir = Workspace.createTempDirectory("flint-run");
            mJob = new FlintRunJob(mOption, executionDir);
            return mService.call(mJob);
    }
}
