/* -*- Mode: Java; tab-width: 4; indent-tabs-mode: nil; c-basic-offset: 4 -*- vim:set ts=4 sw=4 sts=4 et: */
package jp.oist.flint.export;

import jp.oist.flint.executor.Isd2csvJob;
import jp.oist.flint.executor.ProcessWorker;
import jp.oist.flint.form.IFrame;
import jp.oist.flint.udp.IReceiver;
import jp.oist.flint.udp.Monitor;
import java.io.File;
import javax.swing.SwingWorker;

public class ExportWorker extends SwingWorker<Boolean, Boolean> {

    private final IFrame mFrame;
    private final IReceiver mReceiver;
    private final File mSource;
    private final File mTarget;

    public ExportWorker(IFrame frame, IReceiver receiver, File source, File target) {
        mFrame = frame;
        mReceiver = receiver;
        mSource = source;
        mTarget = target;
    }

    public ExportWorker(IFrame frame, File source, File target) {
        this(frame, null, source, target);
    }

    @Override
    protected Boolean doInBackground() throws Exception {
        Isd2csvJob job;
        if (mReceiver == null) {
            job = new Isd2csvJob(mSource, mTarget);
        } else {
            int port = Monitor.getInstance().addReceiver(mReceiver);
            job = new Isd2csvJob(mSource, mTarget, port);
        }
        ProcessWorker worker = new ProcessWorker(job.getProcess(), mFrame);
        worker.execute();
        return job.call().exists() && worker.get() == 0;
    }
}
