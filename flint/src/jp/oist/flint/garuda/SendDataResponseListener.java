/* -*- Mode: Java; tab-width: 4; indent-tabs-mode: nil; c-basic-offset: 4 -*- vim:set ts=4 sw=4 sts=4 et: */
package jp.oist.flint.garuda;

import jp.sbi.garuda.backend.incomingHandler.IncomingResponseProtocolHandler;
import jp.sbi.garuda.backend.incomingHandler.garudaActionListeners.responses.SendDataToGadgetResponseActionListener;
import jp.sbi.garuda.backend.incomingHandler.responseCodes.GarudaResponseCode;
import javax.swing.JFrame;

class SendDataResponseListener
    implements SendDataToGadgetResponseActionListener {

    private final IncomingResponseProtocolHandler mHandler;

    private final JFrame mFrame;

    public SendDataResponseListener(IncomingResponseProtocolHandler handler,
                                    JFrame frame) {
        mHandler = handler;
        mFrame = frame;
    }

    @Override
    public void dataReceivedSuccessfully(String targetGadgetID, String targetGadgetName) {
        mHandler.removeSendDataToGadgetResponseActionListener(this);
        mFrame.setEnabled(true);
    }

    @Override
    public void dataSentNotFound(GarudaResponseCode responseCode, String targetGadgetID, String targetGadgetName) {
        mHandler.removeSendDataToGadgetResponseActionListener(this);
        mFrame.setEnabled(true);
    }

    @Override
    public void gotErrorOnSendDataToGadgetRequest(GarudaResponseCode responseCode, String targetGadgetID, String targetGadgetName) {
        mHandler.removeSendDataToGadgetResponseActionListener(this);
        mFrame.setEnabled(true);
    }

    @Override
    public void incompatibleDataTypeSent(GarudaResponseCode responseCode, String targetGadgetID, String targetGadgetName) {
        mHandler.removeSendDataToGadgetResponseActionListener(this);
        mFrame.setEnabled(true);
    }

    @Override
    public void incompleteRequestParameter(GarudaResponseCode responseCode, String targetGadgetID, String targetGadgetName)  {
        mHandler.removeSendDataToGadgetResponseActionListener(this);
        mFrame.setEnabled(true);
    }
}
