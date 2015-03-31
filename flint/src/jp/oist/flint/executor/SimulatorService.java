/* -*- Mode: Java; tab-width: 4; indent-tabs-mode: nil; c-basic-offset: 4 -*- vim:set ts=4 sw=4 sts=4 et: */
package jp.oist.flint.executor;

import java.io.BufferedWriter;
import java.io.File;
import java.io.FileWriter;
import java.io.IOException;
import java.util.List;
import jp.oist.flint.form.IFrame;

import java.util.concurrent.ExecutorService;
import java.util.concurrent.Executors;
import java.util.concurrent.Future;

public class SimulatorService {

    private final IFrame mFrame;
    private final ExecutorService mPool = Executors.newFixedThreadPool(3);

    public SimulatorService(IFrame frame) {
        mFrame = frame;
    }

    public Future submit(final IJob job) {
        final ProcessWorker processWorker = new ProcessWorker(job.getCommand(), job.getProcess(), mFrame);
        processWorker.execute();
        return mPool.submit(job);
    }

    public Future submit(final IJob job, final File logFile) throws IOException {
        final BufferedWriter writer = new BufferedWriter(new FileWriter(logFile));
        final ProcessWorker processWorker 
                = new ProcessWorker(job.getCommand(), job.getProcess(), mFrame) {
            @Override
            protected void process(List<String> lines) {
                try {
                    for (String line : lines) {
                        writer.append(line).append(System.getProperty("line.separator"));
                    }
                } catch (IOException ex) {
                }
            }

            @Override
            protected void done () {
                try {
                    writer.close();
                } catch (IOException ex) {
                }
                super.done();
            }
        };
        processWorker.execute();
        return mPool.submit(job);
    }
}
