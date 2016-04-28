/* -*- Mode: Java; tab-width: 4; indent-tabs-mode: nil; c-basic-offset: 4 -*- vim:set ts=4 sw=4 sts=4 et: */
package jp.oist.flint.garuda;

import jp.oist.flint.form.MainFrame;
import jp.sbi.garuda.backend.GarudaBackend;
import jp.sbi.garuda.backend.incomingHandler.IncomingResponseProtocolHandler;
import jp.sbi.garuda.backend.net.exception.GarudaConnectionNotInitializedException;
import jp.sbi.garuda.backend.net.exception.NetworkConnectionException;
import java.io.File;
import javax.swing.JFrame;
import org.apache.log4j.Logger;

public class GarudaClient {

    private static final String ID_ON_MACOSX  = "c87f46b3-e74d-4251-ab9f-1b99caac8e2c";
    private static final String ID_ON_WINDOWS = "01a209fc-6a80-11e2-8738-1f7eda18fd79";
    private static final String NAME = "Flint";

    private static GarudaBackend mBackend;

    public static boolean isRunning() {
        return mBackend != null && mBackend.isConnectionLive();
    }

    public static void start(MainFrame frame)
        throws GarudaConnectionNotInitializedException, NetworkConnectionException {
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
        mBackend = new GarudaBackend(id, NAME, frame);
        mBackend.getIncomingRequestHandler().addLoadDataRequestActionListener(new LoadDataRequestListener(frame));
        mBackend.getIncomingResponseHandler().addActivateGadgetResponseActionListener(new ActivateGadgetResponseListener());
        mBackend.activateGadget();
    }

    public static void stop() {
        mBackend.stopService();
    }

    public static void sendFile(File file, String format, JFrame frame) {
        frame.setEnabled(false);
        IncomingResponseProtocolHandler irph = mBackend.getIncomingResponseHandler();
        irph.addGetCompatibleGadgetListResponseActionListener(new CompatibleGadgetListResponseListener(mBackend, irph, frame));
        mBackend.getCompatibleGadgetList(file, format);
    }
}
