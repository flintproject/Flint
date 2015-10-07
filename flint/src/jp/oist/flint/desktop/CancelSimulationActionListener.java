/* -*- Mode: Java; tab-width: 4; indent-tabs-mode: nil; c-basic-offset: 4 -*- vim:set ts=4 sw=4 sts=4 et: */
package jp.oist.flint.desktop;

import jp.oist.flint.form.ProgressCell;
import jp.oist.flint.form.sub.SubFrame;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;

public class CancelSimulationActionListener implements ActionListener {

    private final ProgressCell mCell;

    public CancelSimulationActionListener(ProgressCell cell) {
        mCell = cell;
    }

    @Override
    public void actionPerformed(ActionEvent ae) {
        SubFrame subFrame = mCell.getDocument().getSubFrame();
        if (subFrame.cancelSimulation()) {
            int progress = mCell.getStatusBarProgress();
            mCell.progressFinished("finished", 0, 100, progress);
        }
    }

}
