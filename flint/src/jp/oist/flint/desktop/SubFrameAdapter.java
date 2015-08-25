/* -*- Mode: Java; tab-width: 4; indent-tabs-mode: nil; c-basic-offset: 4 -*- vim:set ts=4 sw=4 sts=4 et: */
package jp.oist.flint.desktop;

import jp.oist.flint.form.sub.SubFrame;
import javax.swing.event.InternalFrameAdapter;
import javax.swing.event.InternalFrameEvent;

class SubFrameAdapter extends InternalFrameAdapter {

    private final Desktop mDesktop;

    public SubFrameAdapter(Desktop desktop) {
        mDesktop = desktop;
    }

    @Override
    public void internalFrameClosing(InternalFrameEvent evt) {
        SubFrame subFrame = (SubFrame)evt.getSource();
        Document document = subFrame.getDocument();
        mDesktop.removeDocument(document);
    }
}
