/* -*- Mode: Java; tab-width: 4; indent-tabs-mode: nil; c-basic-offset: 4 -*- vim:set ts=4 sw=4 sts=4 et: */
package jp.oist.flint.control;

import jp.oist.flint.form.MainFrame;
import java.awt.datatransfer.DataFlavor;
import java.awt.datatransfer.UnsupportedFlavorException;
import java.io.File;
import java.io.IOException;
import java.util.List;
import javax.swing.JComponent;
import javax.swing.TransferHandler;

public class ModelFileTransferHandler extends TransferHandler {
    private MainFrame mMainJFrame;

    public ModelFileTransferHandler(MainFrame mainJFrame) {
        this.mMainJFrame = mainJFrame;
    }

    @Override
    public boolean canImport(TransferSupport support) {
        if (!support.isDrop()) return false;

        if (!support.isDataFlavorSupported(DataFlavor.javaFileListFlavor))
            return false;

        support.setShowDropLocation(true);
        return true;
    }

    @Override
    public int getSourceActions(JComponent c) {
        return COPY_OR_MOVE;
    }

    @Override
    public boolean importData(TransferSupport support) {
        if (!canImport(support)) return false;

        try {
            Object object = support.getTransferable().getTransferData(DataFlavor.javaFileListFlavor);
            List<File> files = (List<File>)object;
            for (File file : files) {
                mMainJFrame.openModel(file);
            }
            return true;
        } catch (UnsupportedFlavorException | IOException ex) {
            // failure
        }
        return false;
    }
}
