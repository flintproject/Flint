/* -*- Mode: Java; tab-width: 4; indent-tabs-mode: nil; c-basic-offset: 4 -*- vim:set ts=4 sw=4 sts=4 et: */
package jp.oist.flint.executor;

import jp.oist.flint.component.Component;
import java.io.IOException;
import java.io.File;
import java.util.concurrent.ExecutionException;

public class Isd2csvJob extends AbstractFileJob {

    public Isd2csvJob(final File isdFile, final File csvFile, int port) throws IOException, InterruptedException, ExecutionException {
        mFile = csvFile;
        mProcessBuilder = new ProcessBuilder(Component.getIsd2csvCommand(isdFile, mFile, port));
        Component.setUpEnvironment(mProcessBuilder);
        mProcessBuilder.redirectErrorStream(true);
        mProcess = mProcessBuilder.start();
    }

}
