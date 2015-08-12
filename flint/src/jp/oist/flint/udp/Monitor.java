/* -*- Mode: Java; tab-width: 4; indent-tabs-mode: nil; c-basic-offset: 4 -*- vim:set ts=4 sw=4 sts=4 et: */
package jp.oist.flint.udp;

import java.io.IOException;
import java.net.InetSocketAddress;
import java.nio.channels.ClosedChannelException;
import java.nio.channels.DatagramChannel;
import java.nio.channels.SelectionKey;
import java.nio.channels.Selector;
import java.nio.channels.spi.SelectorProvider;
import java.util.Iterator;
import java.util.Map;
import java.util.concurrent.ConcurrentHashMap;
import javax.swing.SwingWorker;

public class Monitor extends SwingWorker<Boolean, Void> {

    private static volatile Monitor mInstance = null;

    private final Selector mSelector;

    private final ConcurrentHashMap<IReceiver, DatagramChannel> mQueue;

    private final ConcurrentHashMap<SelectionKey, IReceiver> mMap;

    private Monitor() throws IOException {
        mSelector = SelectorProvider.provider().openSelector();
        mQueue = new ConcurrentHashMap<>();
        mMap = new ConcurrentHashMap<>();
    }

    public static synchronized Monitor getInstance() throws IOException {
        if (mInstance == null) {
            mInstance = new Monitor();
            mInstance.execute();
        }
        return mInstance;
    }

    public int addReceiver(IReceiver receiver) throws IOException {
        assert receiver != null;

        DatagramChannel channel = DatagramChannel.open();
        channel.configureBlocking(false);

        // TODO: the following InetSocketAddress has a wildcard address;
        // it would be better to make it stricter
        InetSocketAddress isaForDatagram = new InetSocketAddress(0);
        channel.socket().bind(isaForDatagram);

        mQueue.put(receiver, channel);
        mSelector.wakeup();
        return channel.socket().getLocalPort();
    }

    private void queue() throws ClosedChannelException {
        for (Map.Entry<IReceiver, DatagramChannel> e : mQueue.entrySet()) {
            SelectionKey key = e.getValue().register(mSelector, SelectionKey.OP_READ);
            mMap.put(key, e.getKey());
        }
        mQueue.clear();
    }

    @Override
    protected Boolean doInBackground() throws IOException {
        boolean result = false;
        int keysAdded;
        while ( (keysAdded = mSelector.select()) >= 0) {
            if (keysAdded == 0) {
                queue();
                continue;
            }
            Iterator<SelectionKey> it = mSelector.selectedKeys().iterator();
            while (it.hasNext()) {
                SelectionKey sk = it.next();
                it.remove(); // remove it from the selected keys because it is never removed automatically

                if (!sk.isValid()) {
                    mMap.remove(sk);
                    continue;
                }

                IReceiver receiver = mMap.get(sk);
                if (receiver != null) {
                    DatagramChannel ready = (DatagramChannel)sk.channel();
                    receiver.receive(ready);
                }
            }
        }
        return result;
    }

    @Override
    protected void done() {
        try {
            mSelector.close();
        } catch (IOException ioe) {
            // ignored
        }
    }
}
