/* -*- Mode: Java; tab-width: 4; indent-tabs-mode: nil; c-basic-offset: 4 -*- vim:set ts=4 sw=4 sts=4 et: */
package jp.oist.flint.garuda;

import jp.sbi.garuda.backend.POJOs.CompatibleGadgetDetails;
import jp.sbi.garuda.backend.incomingHandler.IncomingResponseProtocolHandler;
import jp.sbi.garuda.backend.incomingHandler.garudaActionListeners.responses.GetCompatibleGadgetListResponseActionListener;
import jp.sbi.garuda.backend.incomingHandler.responseCodes.GarudaResponseCode;
import java.util.List;
import javax.swing.JFrame;
import javax.swing.JOptionPane;
import javax.swing.SwingUtilities;

class CompatibleGadgetListResponseListener
    implements GetCompatibleGadgetListResponseActionListener {

    private final IncomingResponseProtocolHandler mHandler;

    private final JFrame mFrame;

    public CompatibleGadgetListResponseListener(IncomingResponseProtocolHandler handler,
                                                JFrame frame) {
        mHandler = handler;
        mFrame = frame;
    }

    @Override
    public void fileNotInOutBoundList(GarudaResponseCode code) {
        mHandler.removeGetCompatibleGadgetListResponseActionListener(this);
        reportError("File not inbound/outbound list: " + code.toString(),
                    "File not inbound/outbound list");
        mFrame.setEnabled(true);
    }

    @Override
    public void gotCompatibleGadgetList(List<CompatibleGadgetDetails> list) {
        mHandler.removeGetCompatibleGadgetListResponseActionListener(this);

        final List<CompatibleGadgetDetails> gadgets = list;
        SwingUtilities.invokeLater(new Runnable() {
            @Override
            public void run() {
                GadgetDialog dialog = new GadgetDialog(mFrame, gadgets);
                dialog.setLocationRelativeTo(mFrame);
                dialog.setVisible(true);
            }
        });
    }

    @Override
    public void gotErrorOnCompatibleGadgetListRequest(GarudaResponseCode responseCode) {
        mHandler.removeGetCompatibleGadgetListResponseActionListener(this);
        reportError("Error on getting compatible gadgets: " + responseCode.toString(),
                    "Error on getting compatible gadgets");
        mFrame.setEnabled(true);
     }

    @Override
    public void noCompatibleGadgetsFound(GarudaResponseCode responseCode) {
        mHandler.removeGetCompatibleGadgetListResponseActionListener(this);
        reportError("No compatible gadgets found: " + responseCode.toString(),
                    "No compatible gadgets found");
        mFrame.setEnabled(true);
    }

    private void reportError(final String message, final String title) {
        SwingUtilities.invokeLater(new Runnable() {
            @Override
            public void run() {
                JOptionPane.showMessageDialog(mFrame,
                                              message,
                                              title,
                                              JOptionPane.ERROR_MESSAGE);
            }
       });
    }
}
