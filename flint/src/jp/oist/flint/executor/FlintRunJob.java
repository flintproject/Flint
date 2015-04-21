/* -*- Mode: Java; tab-width: 4; indent-tabs-mode: nil; c-basic-offset: 4 -*- vim:set ts=4 sw=4 sts=4 et: */

package jp.oist.flint.executor;

import java.io.File;
import java.io.IOException;
import java.io.InputStream;
import java.io.OutputStream;
import java.nio.file.Files;
import java.nio.file.Path;
import java.nio.file.Paths;
import java.nio.file.StandardCopyOption;
import java.util.List;
import jp.oist.flint.component.Component;
import jp.physiome.Cli.RunOption;

public class FlintRunJob implements IJob<Boolean> {

    private final RunOption mOption;
    private final ProcessBuilder mProcessBuilder;
    private final Process mProcess;

    public FlintRunJob(RunOption option, File workingDir) throws IOException {
        mOption = option;
        mProcessBuilder = new ProcessBuilder(Component.getFlintRunCommand());
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

        if (mOption.hasErrorFilename()) {
            InputStream is = mProcess.getInputStream();
            Path path = Paths.get(mOption.getErrorFilename());
            Files.copy(is, path, StandardCopyOption.REPLACE_EXISTING);
        }
        int r = mProcess.waitFor();
        return (r == 0);
    }

    @Override
    public Process getProcess() {return mProcess;}
}
