/* -*- Mode: Java; tab-width: 4; indent-tabs-mode: nil; c-basic-offset: 4 -*- vim:set ts=4 sw=4 sts=4 et: */
package jp.oist.flint.export;

import java.beans.PropertyChangeEvent;
import java.beans.PropertyChangeListener;
import jp.oist.flint.form.sub.JobWindow;
import jp.oist.flint.udp.IReceiver;
import java.io.IOException;
import java.nio.ByteBuffer;
import java.nio.channels.DatagramChannel;
import javax.swing.ProgressMonitor;
import javax.swing.SwingWorker;

public class ExportReceiver implements IReceiver {

    private final ProgressMonitor mProgressMonitor;

    private ExportWorker mWorker;

    public ExportReceiver(JobWindow window) {
        mProgressMonitor = new ProgressMonitor(window,
                                               "Exporting ...",
                                               null,
                                               0,
                                               100);
        mWorker = null;
    }

    public void setWorker(ExportWorker worker) {
        mWorker = worker;
        mWorker.addPropertyChangeListener(new PropertyChangeListener() {
            @Override
            public void propertyChange(PropertyChangeEvent evt) {
                String propertyName = evt.getPropertyName();
                Object nv = evt.getNewValue();
                if ("state".equals(propertyName)
                    && SwingWorker.StateValue.DONE.equals(nv)) {
                    mProgressMonitor.close();
                }
            }
        });
    }

    @Override
    public void receive(DatagramChannel channel) throws IOException {
        if (mProgressMonitor.isCanceled() && mWorker != null) {
            mWorker.cancel(true);
            mWorker = null;
            return;
        }
        ByteBuffer bb = ByteBuffer.allocate(1);
        if (channel.receive(bb) != null) {
            bb.flip();
            int p = bb.get();
            mProgressMonitor.setProgress(p);
        }
    }
}
