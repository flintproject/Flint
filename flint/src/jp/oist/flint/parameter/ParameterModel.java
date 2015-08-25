/* -*- Mode: Java; tab-width: 4; indent-tabs-mode: nil; c-basic-offset: 4 -*- vim:set ts=4 sw=4 sts=4 et: */
package jp.oist.flint.parameter;

import org.apache.log4j.Logger;
import java.io.DataInputStream;
import java.io.EOFException;
import java.io.File;
import java.io.FileInputStream;
import java.io.IOException;
import java.io.RandomAccessFile;
import java.nio.ByteOrder;
import java.nio.DoubleBuffer;
import java.nio.MappedByteBuffer;
import java.nio.channels.FileChannel;
import java.util.ArrayList;
import java.util.List;
import javax.swing.table.AbstractTableModel;
import jp.physiome.Lo;
import jp.physiome.Ipc;

public abstract class ParameterModel extends AbstractTableModel {

    public static ParameterModel factory (Ipc.ModelLanguage lang, File paramFile, File dataFile)
        throws IOException {
        switch (lang) {
            case ISML:
                return new PhmlParameterModel(paramFile, dataFile);
            case SBML:
                return new SbmlParameterModel(paramFile, dataFile);
            default:
        }
        return null;
    }

    protected Object[] mColumns;

    protected Lo.Header mHeader = null;

    protected List<Parameter> mParameterList = null;

    public ParameterModel (File paramFile, File dataFile) throws IOException {

        try (DataInputStream dis = new DataInputStream(new FileInputStream(paramFile))) {

        int len = dis.readInt();
        if (len <= 0)
            Logger.getRootLogger().error("invalid length of Lo.Header : " + len);
        byte[] buf = new byte[len];
        dis.readFully(buf);

        mHeader = Lo.Header.parseFrom(buf);
        mParameterList = new ArrayList<>();

        try (FileInputStream fis = new FileInputStream(dataFile);
             FileChannel fc = fis.getChannel()) {
        MappedByteBuffer mbb = fc.map(FileChannel.MapMode.READ_ONLY, 0, mHeader.getSize() * 8);
        mbb.order(ByteOrder.LITTLE_ENDIAN); // FIXME
        DoubleBuffer db = mbb.asDoubleBuffer();

        while (true) {
            try {
                len = dis.readInt();

                if (len <= 0) {
                    Logger.getRootLogger().error("invalid length of Lo.Header : " + len);
                    break;
                }
            } catch (EOFException ex) {
                break;
            }
            buf = new byte[len];
            dis.readFully(buf);
            Lo.Column column = Lo.Column.parseFrom(buf);
            double value = db.get(column.getPosition());
            Parameter parameter = new Parameter(column, value);

            mParameterList.add(parameter);
        }
        }
        }
    }

    public void save(File file) throws IOException {
        try (RandomAccessFile raf = new RandomAccessFile(file, "rw");
             FileChannel fc = raf.getChannel()) {
        MappedByteBuffer mbb = fc.map(FileChannel.MapMode.READ_WRITE, 0, mHeader.getSize() * 8);
        mbb.order(ByteOrder.nativeOrder()); // TODO
        DoubleBuffer db = mbb.asDoubleBuffer();

        for (Parameter parameter : mParameterList) {
            Double value = Double.parseDouble(parameter.getValue());
            db.put(parameter.getPosition(), value); // FIXME: size
        }
        }
    }

    @Override
    public int getRowCount() {
        return mParameterList.size();
    }

    @Override
    public int getColumnCount() {
        return mColumns.length;
    }

    @Override
    public String getColumnName(int column) {
        return mColumns[column].toString();
    }

    @Override
    public Class<?> getColumnClass(int columnIndex) {
        return String.class;
    }
}
