/* -*- Mode: Java; tab-width: 4; indent-tabs-mode: nil; c-basic-offset: 4 -*- vim:set ts=4 sw=4 sts=4 et: */
package jp.oist.flint.util;

import jp.oist.flint.control.VariableList;
import org.apache.log4j.Logger;
import java.awt.datatransfer.DataFlavor;
import java.awt.datatransfer.Transferable;
import java.awt.datatransfer.UnsupportedFlavorException;
import java.io.IOException;
import javax.activation.ActivationDataFlavor;
import javax.activation.DataHandler;
import javax.swing.JComponent;
import javax.swing.TransferHandler;

/**
 * This is the TransferHandler class for VariableList.
 */
public class ListItemTransferHandler extends TransferHandler {

    /** DataFlavor Object. */
    private final DataFlavor localObjectFlavor;
    /** Transfered Object. */
    private Object[] transferedObjects = null;
    /** The selected indices Object. */
    private int[] indices = null;
    /** DataFlavor Object. */
    private int addIndex = -1;
    /** The VariableList object of a moved material. */
    private VariableList source = null;

    public ListItemTransferHandler() {
        localObjectFlavor = new ActivationDataFlavor(Object[].class, DataFlavor.javaJVMLocalObjectMimeType, "Array of items");
    }

    /**
     * Creates a Transferable to use as the source for a data transfer.
     *
     * @see TransferHandler
     * @param c JComponent
     * @return Transferable
     */
    @Override
    protected Transferable createTransferable(JComponent c) {
        source = (VariableList) c;
        indices = source.getSelectedIndices();
        transferedObjects = source.getSelectedValues();
        return new DataHandler(transferedObjects, localObjectFlavor.getMimeType());
    }

    /**
     *  Indicates whether a component would accept an import of the given set of data flavors prior to actually attempting to import it.
     *
     * @see TransferHandler
     * @param info TransferSupport
     * @return boolean
     */
    @Override
    public boolean canImport(TransferHandler.TransferSupport info) {
        // do not allow to drop them into the original component
        return (VariableList)info.getComponent() != source && info.isDrop() && info.isDataFlavorSupported(localObjectFlavor);
    }

    /**
     *  Returns the type of transfer actions supported by the source.
     *
     * @see TransferHandler
     * @param c JComponent
     * @return int
     */
    @Override
    public int getSourceActions(JComponent c) {
        return TransferHandler.MOVE;
    }

    /**
     * Processing of the Drop point.
     *
     * @see TransferHandler
     * @param info TransferSupport
     * @return boolean
     */
    @Override
    public boolean importData(TransferHandler.TransferSupport info) {
        if (!canImport(info)) {
            return false;
        }
        VariableList target = (VariableList) info.getComponent();
        VariableList.DropLocation dl = (VariableList.DropLocation) info.getDropLocation();
        ListItemModel listModel = (ListItemModel) target.getModel();
        int index = dl.getIndex();

        int max = listModel.getSize();
        if (index < 0 || index > max) {
            index = max;
        }
        addIndex = index;

        try {
            Object[] values = (Object[]) info.getTransferable().getTransferData(localObjectFlavor);

            if (!listModel.getMultipleFlg() && values.length > 1) {
                return false;
            }

            ListItemModel sourceModel = (ListItemModel) source.getModel();
            target.clearSelection();

            if (!listModel.getMultipleFlg() && listModel.getSize() > 0) {
                String key = listModel.firstKey();
                String name = listModel.firstName();
                listModel.remove(0);
                sourceModel.add(key, name);
            }

            for (int i = 0; i < indices.length; i++) {
                int idx = index++;
                listModel.add(sourceModel.keyAt(indices[i]),
                              sourceModel.nameAt(indices[i]));

                target.addSelectionInterval(idx, idx);
            }
            return true;
        } catch (UnsupportedFlavorException | IOException ioe) {
            Logger.getRootLogger().error(ioe.getMessage());
        }
        return false;
    }

    /**
     * Invoked after data has been exported.
     *
     * @see TransferHandler
     * @param c JComponent
     * @param data Transferable
     * @param action int
     */
    @Override
    protected void exportDone(JComponent c, Transferable data, int action) {
        cleanup(c, action == TransferHandler.MOVE);
    }

    /**
     * Processing after movement of an object.
     *
     * @param c JComponent
     * @param remove boolean
     */
    private void cleanup(JComponent c, boolean remove) {
        if (remove && indices != null) {
            VariableList sourceList = (VariableList) c;
            ListItemModel model = (ListItemModel) sourceList.getModel();

            sourceList.clearSelection();
            for (int i = indices.length - 1; i >= 0; i--) {
                model.remove(indices[i]);
            }
        }
        indices = null;
        addIndex = -1;
    }
}
