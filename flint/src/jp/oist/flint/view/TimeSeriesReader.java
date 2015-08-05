/* -*- Mode: Java; tab-width: 4; indent-tabs-mode: nil; c-basic-offset: 4 -*- vim:set ts=4 sw=4 sts=4 et: */
package jp.oist.flint.view;

import jp.oist.flint.isdf.IsdfException;
import jp.oist.flint.isdf.IsdfHeader;
import jp.oist.flint.isdf.IsdfReader;
import org.apache.log4j.Logger;
import org.jfree.data.xy.XYSeries;
import java.io.File;
import java.io.FileInputStream;
import java.io.IOException;
import java.nio.ByteBuffer;
import java.nio.ByteOrder;
import java.nio.channels.FileChannel;
import java.util.ArrayList;
import java.util.TreeMap;
import javax.swing.SwingWorker;

public class TimeSeriesReader extends SwingWorker<ArrayList<XYSeries>, Void> {

    private final File mIsdFile;
    private final TreeMap<Integer, Integer> mMap = new TreeMap<>();
    private final ArrayList<XYSeries> mResult = new ArrayList<>();

    public TimeSeriesReader(File isdFile, ArrayList<Integer> indices, ArrayList<String> legends) {
        mIsdFile = isdFile;
        for (int i = 0; i < indices.size(); i++) {
            mMap.put(indices.get(i), i);
        }
        for (String legend : legends) {
            mResult.add(new XYSeries(legend));
        }
    }

    public TimeSeriesReader(File isdFile, int index, String legend) {
        mIsdFile = isdFile;
        mMap.put(index, 0);
        mResult.add(new XYSeries(legend));
    }

    @Override
    protected ArrayList<XYSeries> doInBackground() throws IOException, InterruptedException, IsdfException {
        int max = 0;
        for (Integer key : mMap.keySet()) {
            if (max < key) {
                max = key;
            }
        }

        try (FileInputStream ifs = new FileInputStream(mIsdFile);
             FileChannel fc = ifs.getChannel()) {
        IsdfReader reader = new IsdfReader(fc);
        IsdfHeader header = reader.readHeader();
        reader.seekData(header);
        int numberOfColumns = header.getNumObjs();
        int numberOfBytes = numberOfColumns * 8;
        ByteBuffer bb = ByteBuffer.allocateDirect(numberOfBytes);
        if (header.isLittleEndian()) bb.order(ByteOrder.LITTLE_ENDIAN);
        long size = fc.size();
        while (fc.position() + numberOfBytes <= size) {
            bb.rewind();
            int r = fc.read(bb);
            if (r != numberOfBytes) {
                Logger.getRootLogger().error("TimeSeriesReader: could not read enough bytes");
                return mResult;
            }
            bb.rewind();

            double d0 = 0.0;
            for (int i = 0; i < numberOfColumns; i++) {
                Double d = bb.getDouble();
                if (i == 0) d0 = d;
                Integer k = mMap.get(i);
                if (k != null) {
                    mResult.get(k).add(d0, d.doubleValue());
                }
                if (i == max) break;
            }
        }
        return mResult;
        }
    }

}
