/* -*- Mode: Java; tab-width: 4; indent-tabs-mode: nil; c-basic-offset: 4 -*- vim:set ts=4 sw=4 sts=4 et: */
package jp.oist.flint.isdf;

import java.io.IOException;
import java.nio.channels.FileChannel;
import java.nio.ByteBuffer;
import java.nio.ByteOrder;

public class IsdfReader {

    private final FileChannel mChannel;

    public IsdfReader(FileChannel channel) {
        mChannel = channel;
    }

    public IsdfHeader ReadHeader() throws IOException, IsdfException {
        ByteBuffer hbb = ByteBuffer.allocateDirect(IsdfHeader.SIZE);
        int r = mChannel.read(hbb);
        if (r != IsdfHeader.SIZE) {
            throw new IsdfException("could not read ISDF header");
        }
        boolean le = (hbb.get(5) == 1);
        if (le) hbb.order(ByteOrder.LITTLE_ENDIAN);
        int numObjs = hbb.getInt(IsdfHeader.SIZE-16);
        int numBytesComment = hbb.getInt(IsdfHeader.SIZE-12);
        int numBytesDescs = hbb.getInt(IsdfHeader.SIZE-8);
        int numBytesUnits = hbb.getInt(IsdfHeader.SIZE-4);
        return new IsdfHeader(le, numObjs, numBytesComment, numBytesDescs, numBytesUnits);
    }

    public void SeekData(IsdfHeader header) throws IOException {
        mChannel.position(header.getDataOffset());
    }

}
