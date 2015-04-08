/* -*- Mode: Java; tab-width: 4; indent-tabs-mode: nil; c-basic-offset: 4 -*- vim:set ts=4 sw=4 sts=4 et: */
package jp.oist.flint.garuda;

import jp.oist.flint.form.MainFrame;
import jp.sbi.garuda.client.backend.BackendAlreadyInitializedException;
import jp.sbi.garuda.client.backend.GarudaClientBackend;
import jp.sbi.garuda.platform.commons.Gadget;
import jp.sbi.garuda.platform.commons.exception.NetworkException;
import jp.sbi.garuda.platform.commons.net.GarudaConnectionNotInitializedException;
import java.io.File;
import org.apache.log4j.Logger;

public class GarudaClient {

    private static final String ID_ON_MACOSX  = "c87f46b3-e74d-4251-ab9f-1b99caac8e2c";
    private static final String ID_ON_WINDOWS = "01a209fc-6a80-11e2-8738-1f7eda18fd79";
    private static final String NAME = "Flint";

    private static GarudaClientBackend mGarudaClientBackend;

    private static GarudaListener mGarudaListener;

    public static boolean isRunning() {
        return mGarudaClientBackend instanceof GarudaClientBackend && mGarudaClientBackend.isInitialized();
    }

    public static void start(MainFrame frame)
        throws GarudaConnectionNotInitializedException, NetworkException {
        String osName = System.getProperty("os.name");
        if (osName == null) {
            Logger.getRootLogger().fatal("could not detect OS name");
            System.exit(1); // never return
        }
        String id;
        if (osName.startsWith("Windows")) {
            id = ID_ON_WINDOWS;
        } else { // Mac OS X and others
            id = ID_ON_MACOSX;
        }
        mGarudaClientBackend = new GarudaClientBackend(id, NAME);
        mGarudaListener = new GarudaListener(frame, mGarudaClientBackend);
        mGarudaClientBackend.addGarudaChangeListener(mGarudaListener);
        try {
            mGarudaClientBackend.initialize();
        } catch (BackendAlreadyInitializedException baie) {
            // cannot happen and can be ignored
        }
    }

    public static void stop() {
        mGarudaClientBackend.stopBackend();
    }

    public static void requestForLoadableGadgets(ICompatibleGadgetClient client,
                                                 String type)
        throws GarudaConnectionNotInitializedException {
        mGarudaListener.requestForLoadableGadgets(client, type);
    }

    public static void sentFileToGadget(Gadget gadget, File file)
        throws NetworkException {
        mGarudaClientBackend.sentFileToGadget(gadget, file);
    }
}
