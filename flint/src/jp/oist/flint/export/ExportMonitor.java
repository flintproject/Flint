/* -*- Mode: Java; tab-width: 4; indent-tabs-mode: nil; c-basic-offset: 4 -*- vim:set ts=4 sw=4 sts=4 et: */
package jp.oist.flint.export;

import java.io.IOException;
import java.net.InetSocketAddress;
import java.nio.ByteBuffer;
import java.nio.channels.ClosedChannelException;
import java.nio.channels.DatagramChannel;
import java.nio.channels.SelectionKey;
import java.nio.channels.Selector;
import java.nio.channels.spi.SelectorProvider;
import java.util.Iterator;
import javax.swing.SwingWorker;

public class ExportMonitor extends SwingWorker<Boolean, Boolean> {

    private final DatagramChannel mDatagramChannel;
    private final Selector mSelector;
    private final SelectionKey mKeyForDatagram;

    public ExportMonitor() throws IOException {
        mDatagramChannel = DatagramChannel.open();
        mDatagramChannel.configureBlocking(false);

        // TODO: the following InetSocketAddress has a wildcard address;
        // it would be better to make it stricter
        InetSocketAddress isaForDatagram = new InetSocketAddress(0);
        mDatagramChannel.socket().bind(isaForDatagram);

        mSelector = SelectorProvider.provider().openSelector();
        mKeyForDatagram = mDatagramChannel.register(mSelector, SelectionKey.OP_READ);
    }

    public int getPort() {
        return mDatagramChannel.socket().getLocalPort();
    }

    @Override
    protected Boolean doInBackground() throws ClosedChannelException, IOException {
        boolean done = false;
        Boolean result = Boolean.FALSE;
        int keysAdded;
        while ( (keysAdded = mSelector.select()) >= 0) {
            if (keysAdded == 0) continue;
            Iterator<SelectionKey> it = mSelector.selectedKeys().iterator();
            while (it.hasNext()) {
                SelectionKey sk = it.next();
                it.remove(); // remove it from the selected keys because it is never removed automatically

                if (sk == mKeyForDatagram) {
                    DatagramChannel ready = (DatagramChannel)sk.channel();
                    ByteBuffer bb = ByteBuffer.allocate(1);
                    if (ready.receive(bb) != null) {
                        bb.flip();
                        int p = bb.get();
                        setProgress(p);
                    }
                }
            }
        }
        return result;
    }

    @Override
    protected void done() {
        try {
            mDatagramChannel.close();
        } catch (IOException ioe) {
            // nothing to do
        }
    }
}
