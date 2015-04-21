/* -*- Mode: Java; tab-width: 4; indent-tabs-mode: nil; c-basic-offset: 4 -*- vim:set ts=4 sw=4 sts=4 et: */
package jp.oist.flint.export;

import jp.oist.flint.executor.Isd2csvJob;
import jp.oist.flint.executor.ProcessWorker;
import jp.oist.flint.form.IFrame;
import java.io.File;
import java.io.IOException;
import java.util.concurrent.ExecutionException;
import java.util.concurrent.ExecutorService;
import java.util.concurrent.Executors;
import javax.swing.SwingWorker;

public class ExportWorker extends SwingWorker<Boolean, Boolean> {

    private final IFrame mFrame;
    private final File mSource;
    private final File mTarget;

    private final ExportMonitor mMonitor = new ExportMonitor();
    private final ExecutorService mPool = Executors.newFixedThreadPool(1);

    public ExportWorker(IFrame frame, File source, File target) throws IOException {
        mFrame = frame;
        mSource = source;
        mTarget = target;
    }

    public ExportMonitor getMonitor() {
        return mMonitor;
    }

    @Override
    protected Boolean doInBackground() throws ExecutionException, IOException, InterruptedException {
        mMonitor.execute();
        Isd2csvJob job = new Isd2csvJob(mSource, mTarget, mMonitor.getPort());
        ProcessWorker worker = new ProcessWorker(job.getProcess(), mFrame);
        worker.execute();
        mPool.submit(job);
        return worker.get() == 0;
    }

    @Override
    protected void done() {
        mMonitor.cancel(false);
    }
}
