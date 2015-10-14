/* -*- Mode: Java; tab-width: 4; indent-tabs-mode: nil; c-basic-offset: 4 -*- vim:set ts=4 sw=4 sts=4 et: */
package jp.oist.flint.form;

import jp.oist.flint.form.sub.SubFrame;
import java.awt.event.MouseEvent;
import java.awt.event.MouseListener;
import java.beans.PropertyVetoException;

class ProgressCellMouseListener implements MouseListener {

    private final SubFrame mSubFrame;

    public ProgressCellMouseListener(SubFrame subFrame) {
        mSubFrame = subFrame;
    }

    @Override
    public void mouseClicked(MouseEvent e) {
        try {
            if (mSubFrame.isIcon())
                mSubFrame.setIcon(false);
            mSubFrame.setSelected(true);
        } catch (PropertyVetoException ex) {
            // ignored
        }
    }

    @Override
    public void mousePressed(MouseEvent e) {
    }

    @Override
    public void mouseReleased(MouseEvent e) {
    }

    @Override
    public void mouseEntered(MouseEvent e) {
    }

    @Override
    public void mouseExited(MouseEvent e) {
    }
}
