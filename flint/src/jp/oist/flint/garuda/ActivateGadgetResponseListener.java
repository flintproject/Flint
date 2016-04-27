/* -*- Mode: Java; tab-width: 4; indent-tabs-mode: nil; c-basic-offset: 4 -*- vim:set ts=4 sw=4 sts=4 et: */
package jp.oist.flint.garuda;

import jp.sbi.garuda.backend.incomingHandler.garudaActionListeners.responses.ActivateGadgetResponseActionListener;
import jp.sbi.garuda.backend.incomingHandler.responseCodes.GarudaResponseCode;

class ActivateGadgetResponseListener
    implements ActivateGadgetResponseActionListener {

    public ActivateGadgetResponseListener() {
    }

    @Override
    public void gadgetActivated() {
        // ignored
    }

    @Override
    public void gadgetActivationFailed(GarudaResponseCode code) {
        // ignored
    }
}
