/* -*- Mode: Java; tab-width: 4; indent-tabs-mode: nil; c-basic-offset: 4 -*- vim:set ts=4 sw=4 sts=4 et: */

package jp.oist.flint.executor;

import java.io.File;
import java.io.IOException;
import java.io.OutputStream;
import java.util.List;
import jp.oist.flint.component.Component;


public class FlintExecJob implements IJob<Boolean> {

    private final File mSedmlFile;

    private final File mPhspFile;

    private final ProcessBuilder mProcessBuilder;

    private final Process mProcess;

    public FlintExecJob (File sedmlFile, File phspFile, File workingDir) 
            throws IOException {
        mSedmlFile = sedmlFile;
        mPhspFile  = phspFile;
        mProcessBuilder = new ProcessBuilder(Component.getFlintExecCommand());
        Component.setUpEnvironment(mProcessBuilder);
        mProcessBuilder.directory(workingDir);
        mProcessBuilder.redirectErrorStream(true);
        mProcess = mProcessBuilder.start();
    }

    @Override
    public Boolean call() throws IOException, InterruptedException, JobException {
        byte[] sedmlBytes = mSedmlFile.getAbsolutePath().getBytes("UTF-8");
        byte[] phspBytes = mPhspFile.getAbsolutePath().getBytes("UTF-8");
        try (OutputStream os = mProcess.getOutputStream()) {
        os.write(sedmlBytes);
        os.write(0);
        os.write(phspBytes);
        os.write(0);
        }
        int r = mProcess.waitFor();
        if (r == 0) return true;

        throw new JobException(this);
    }

    @Override
    public Process getProcess() {
        return mProcess;
    }
}
