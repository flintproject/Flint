/* -*- Mode: Java; tab-width: 4; indent-tabs-mode: nil; c-basic-offset: 4 -*- vim:set ts=4 sw=4 sts=4 et: */
package jp.oist.flint.executor;

import java.io.BufferedWriter;
import java.io.File;
import java.io.FileOutputStream;
import java.io.IOException;
import java.io.OutputStreamWriter;
import java.nio.charset.StandardCharsets;
import java.util.List;
import jp.oist.flint.form.IFrame;

public class SimulatorService {

    private final IFrame mFrame;

    public SimulatorService(IFrame frame) {
        mFrame = frame;
    }

    public <T> T call(final IJob<T> job) throws Exception {
        final ProcessWorker processWorker = new ProcessWorker(job.getProcess(), mFrame);
        processWorker.execute();
        return job.call();
    }

    public <T> T call(final IJob<T> job, final File logFile) throws Exception {
        final ProcessWorker processWorker = new ProcessWorker(job.getProcess(), mFrame) {
            @Override
            protected void process(List<String> lines) {
                // append lines to the log file
                try (FileOutputStream fos = new FileOutputStream(logFile, true);
                     OutputStreamWriter osw = new OutputStreamWriter(fos, StandardCharsets.UTF_8);
                     BufferedWriter writer = new BufferedWriter(osw)) {
                    for (String line : lines) {
                        writer.append(line).append(System.getProperty("line.separator"));
                    }
                } catch (IOException ex) {
                }
            }
        };
        processWorker.execute();
        return job.call();
    }
}
