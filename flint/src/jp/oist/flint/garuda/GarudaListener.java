/* -*- Mode: Java; tab-width: 4; indent-tabs-mode: nil; c-basic-offset: 4 -*- vim:set ts=4 sw=4 sts=4 et: */
package jp.oist.flint.garuda;

import jp.oist.flint.form.IMainFrame;
import jp.sbi.garuda.client.backend.GarudaClientBackend;
import jp.sbi.garuda.client.backend.listeners.GarudaBackendPropertyChangeEvent;
import jp.sbi.garuda.platform.commons.Gadget;
import jp.sbi.garuda.platform.commons.net.GarudaConnectionNotInitializedException;
import org.apache.log4j.Logger;
import java.beans.PropertyChangeEvent;
import java.beans.PropertyChangeListener;
import java.io.File;

public class GarudaListener implements PropertyChangeListener {

    private final IMainFrame mFrame;
    private final GarudaClientBackend mGarudaClientBackend;

    private ICompatibleGadgetClient mCompatibleGadgetClient;
    private String mCompatibleType;

    public GarudaListener(IMainFrame frame, GarudaClientBackend backend) {
        mFrame = frame;
        mGarudaClientBackend = backend;
        mCompatibleGadgetClient = null;
        mCompatibleType = null;
    }

    public void requestForLoadableGadgets(ICompatibleGadgetClient client,
                                          String type)
        throws GarudaConnectionNotInitializedException {
        mCompatibleGadgetClient = client;
        mCompatibleType = type;
        mGarudaClientBackend.requestForLoadableGadgets(type, type);
    }

    @Override
    public void propertyChange(PropertyChangeEvent e) {
        if (e instanceof GarudaBackendPropertyChangeEvent) {
            GarudaBackendPropertyChangeEvent evt = (GarudaBackendPropertyChangeEvent)e;
            String propertyName = evt.getPropertyName();
            if (propertyName.equals(GarudaClientBackend.LOAD_DATA_PROPERTY_CHANGE_ID)) {
                Gadget gadget = (Gadget)evt.getFirstProperty();
                final String path = (String)evt.getSecondProperty();
                File file = new File(path);
                if (!file.isFile()) {
                    mGarudaClientBackend.sentLoadDataResponse(false, gadget);
                    return;
                }
                if (mFrame.openModel(file)) {
                    mGarudaClientBackend.sentLoadDataResponse(true, gadget);
                } else {
                    mGarudaClientBackend.sentLoadDataResponse(false, gadget);
                }
            } else if (propertyName.equals(GarudaClientBackend.CONNECTION_NOT_INITIALIZED_ID)) {
                mFrame.showErrorDialog("Could not establish connection to Garuda", "Error on connecting to Garuda");
            } else if (propertyName.equals(GarudaClientBackend.GADGET_REGISTRATION_ERROR_ID)) {
                Logger.getRootLogger().error("error no registering to Garuda");
            } else if (propertyName.equals(GarudaClientBackend.GADGET_REGISTRERED_ID)) {
                Logger.getRootLogger().error("registered to Garuda");
            } else if (propertyName.equals(GarudaClientBackend.GOT_GADGETS_PROPERTY_CHANGE_ID)) {
                if (mCompatibleGadgetClient == null) return; // nothing to do
                if ("csv".equalsIgnoreCase(mCompatibleType)) {
                    mCompatibleGadgetClient.loadGadgetsForCsv(mGarudaClientBackend.getCompatibleGadgetList());
                } else if ("isd".equalsIgnoreCase(mCompatibleType)) {
                    mCompatibleGadgetClient.loadGadgetsForIsd(mGarudaClientBackend.getCompatibleGadgetList());
                }
                mCompatibleGadgetClient = null; // stop listening
            } else {
                Logger.getRootLogger().error("unexpected property name: " + propertyName);
            }
        }
    }
}
