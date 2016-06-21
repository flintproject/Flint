/* -*- Mode: Java; tab-width: 4; indent-tabs-mode: nil; c-basic-offset: 4 -*- vim:set ts=4 sw=4 sts=4 et: */
package jp.oist.flint.executor;

import java.io.File;
import java.io.IOException;
import java.io.OutputStream;
import jp.oist.flint.component.Component;
import jp.physiome.Cli.RunOption;

public class FlintTrJob implements IJob<Boolean> {

    private final RunOption mOption;
    private final ProcessBuilder mProcessBuilder;
    private final Process mProcess;

    public FlintTrJob(RunOption option, File workingDir) throws IOException {
        mOption = option;
        mProcessBuilder = new ProcessBuilder(Component.getFlintTrCommand());
        Component.setUpEnvironment(mProcessBuilder);
        mProcessBuilder.directory(workingDir);
        mProcessBuilder.redirectErrorStream(true);
        mProcess = mProcessBuilder.start();
    }

    @Override
    public Boolean call() throws IOException, InterruptedException {
        try (OutputStream os = mProcess.getOutputStream()) {
            mOption.writeTo(os);
        }
        return mProcess.waitFor() == 0;
    }

    @Override
    public Process getProcess() {
        return mProcess;
    }
}
