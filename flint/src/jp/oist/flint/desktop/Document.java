/* -*- Mode: Java; tab-width: 4; indent-tabs-mode: nil; c-basic-offset: 4 -*- vim:set ts=4 sw=4 sts=4 et: */
package jp.oist.flint.desktop;

import jp.oist.flint.form.sub.SubFrame;
import jp.physiome.Ipc;
import java.io.File;
import java.util.Objects;

public class Document {

    private final File mFile;

    private final Ipc.ModelProbeResponse mResponse;

    private final File mDirectory;

    private SubFrame mSubFrame;

    public Document(File file, Ipc.ModelProbeResponse response, File directory) {
        mFile = file;
        mResponse = response;
        mDirectory = directory;
        mSubFrame = null;
    }

    public File getFile() {
        return mFile;
    }

    public Ipc.ModelProbeResponse getResponse() {
        return mResponse;
    }

    public File getDirectory() {
        return mDirectory;
    }

    public void setSubFrame(SubFrame subFrame) {
        mSubFrame = subFrame;
    }

    public SubFrame getSubFrame() {
        return mSubFrame;
    }

    @Override
    public boolean equals(Object obj) {
        if (!(obj instanceof Document))
            return false;
        Document doc = (Document)obj;
        return mFile.equals(doc.mFile) && mDirectory.equals(doc.mDirectory);
    }

    @Override
    public int hashCode() {
        return Objects.hash(mFile, mDirectory);
    }
}
