/* -*- Mode: Java; tab-width: 4; indent-tabs-mode: nil; c-basic-offset: 4 -*- vim:set ts=4 sw=4 sts=4 et: */
package jp.oist.flint.form.job;

import jp.oist.flint.form.IFrame;
import jp.sbi.garuda.platform.commons.Gadget;
import java.io.File;
import java.util.List;
import javax.swing.ListSelectionModel;
import javax.swing.event.ListSelectionEvent;
import javax.swing.event.ListSelectionListener;

public class GadgetSelectionListener implements ListSelectionListener {

    final GadgetDialog mDialog;
    final IFrame mFrame;
    final List<Gadget> mGadgets;
    final File mFile;
    final String mExtension;

    public GadgetSelectionListener(GadgetDialog dialog,
                                   IFrame frame,
                                   List<Gadget> gadgets,
                                   File file,
                                   String extension) {
        mDialog = dialog;
        mFrame = frame;
        mGadgets = gadgets;
        mFile = file;
        mExtension = extension;
    }

    @Override
    public void valueChanged(ListSelectionEvent event) {
        if (event.getValueIsAdjusting()) return; // ignore it
        ListSelectionModel lsm = (ListSelectionModel)event.getSource();
        if (lsm.isSelectionEmpty()) return; // ignore it
        int minIndex = lsm.getMinSelectionIndex();
        int maxIndex = lsm.getMaxSelectionIndex();
        for (int i = minIndex; i <= maxIndex; i++) {
            if (lsm.isSelectedIndex(i)) {
                mDialog.dispose();
                GadgetFeeder feeder = new GadgetFeeder(mFrame,
                                                       mGadgets.get(i),
                                                       mFile,
                                                       mExtension);
                feeder.execute();
                return;
            }
        }
    }
}
