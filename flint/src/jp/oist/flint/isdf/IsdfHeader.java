/* -*- Mode: Java; tab-width: 4; indent-tabs-mode: nil; c-basic-offset: 4 -*- vim:set ts=4 sw=4 sts=4 et: */
package jp.oist.flint.isdf;

public class IsdfHeader {

    static public final int SIZE = 44;

    private final boolean mLittleEndian;
    private final int mNumObjs;
    private final int mNumBytesComment;
    private final int mNumBytesDescs;
    private final int mNumBytesUnits;

    public IsdfHeader(boolean le, int numObjs, int numBytesComment, int numBytesDescs, int numBytesUnits) {
        mLittleEndian = le;
        mNumObjs = numObjs;
        mNumBytesComment = numBytesComment;
        mNumBytesDescs = numBytesDescs;
        mNumBytesUnits = numBytesUnits;
    }

    public boolean isLittleEndian() {
        return mLittleEndian;
    }

    public int getNumObjs() {
        return mNumObjs;
    }

    public int getNumBytesComment() {
        return mNumBytesComment;
    }

    public int getNumBytesDescs() {
        return mNumBytesDescs;
    }

    public int getNumBytesUnits() {
        return mNumBytesUnits;
    }

    public int getDataOffset() {
        return SIZE + mNumBytesComment + mNumBytesDescs + mNumBytesUnits;
    }

}
