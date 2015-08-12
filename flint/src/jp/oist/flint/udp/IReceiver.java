/* -*- Mode: Java; tab-width: 4; indent-tabs-mode: nil; c-basic-offset: 4 -*- vim:set ts=4 sw=4 sts=4 et: */
package jp.oist.flint.udp;

import java.io.IOException;
import java.nio.channels.DatagramChannel;

public interface IReceiver {

    void receive(DatagramChannel channel) throws IOException;

}
