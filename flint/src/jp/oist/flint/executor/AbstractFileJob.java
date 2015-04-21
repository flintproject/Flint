/* -*- Mode: Java; tab-width: 4; indent-tabs-mode: nil; c-basic-offset: 4 -*- vim:set ts=4 sw=4 sts=4 et: */
package jp.oist.flint.executor;

import java.io.File;
import java.io.IOException;
import java.util.List;

public class AbstractFileJob implements IJob<File> {

    protected File mFile;
    protected ProcessBuilder mProcessBuilder;
    protected Process mProcess;

    @Override
    public File call() throws IOException, InterruptedException, JobException {
        int r = mProcess.waitFor();
        if (r == 0) return mFile;
        throw new JobException(this);
    }

    @Override
    public Process getProcess() {return mProcess;}

}
