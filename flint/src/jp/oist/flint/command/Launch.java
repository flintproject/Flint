/* -*- Mode: Java; tab-width: 4; indent-tabs-mode: nil; c-basic-offset: 4 -*- vim:set ts=4 sw=4 sts=4 et: */
package jp.oist.flint.command;

import jp.oist.flint.form.MainFrame;
import jp.oist.flint.garuda.GarudaClient;
import jp.oist.flint.rpc.Server;
import jp.sbi.garuda.platform.commons.exception.NetworkException;
import jp.sbi.garuda.platform.commons.net.GarudaConnectionNotInitializedException;
import org.apache.log4j.Logger;
import java.awt.Color;
import java.io.File;
import java.io.IOException;
import javax.swing.UIManager;
import javax.swing.UnsupportedLookAndFeelException;

public class Launch implements Runnable {

    private final CommandLineArguments mCommandLineArgs;
    private final Session mSession;

    private MainFrame mMainFrame;

    public Launch(final CommandLineArguments commandLineArgs) {
        mCommandLineArgs = commandLineArgs;
        mSession = new Session();
        mMainFrame = null;
    }

    @Override
    public void run() {
        try {
            UIManager.setLookAndFeel(UIManager.getSystemLookAndFeelClassName());
        } catch (UnsupportedLookAndFeelException | ClassNotFoundException | InstantiationException | IllegalAccessException e) {
            // ignored; giving up controlling Look&Feel
        }
        UIManager.put("Desktop.background", Color.WHITE);

        try {
            mMainFrame = new MainFrame(mSession);
            mMainFrame.startWatching();
        } catch (IOException ex) {
            Logger.getRootLogger().fatal("could not launch " + MainFrame.class.getName() + ": " + ex.getMessage());
            System.exit(1);
            return;
        }

        Runtime.getRuntime().addShutdownHook(new Rescue(mSession));

        try {
            Server server = new Server(mMainFrame);
            server.start();
        } catch (IOException ioe) {
            Logger.getRootLogger().fatal("could not start " + Server.class.getName() + ": " + ioe.getMessage());
            System.exit(1);
            return;
        }

        mMainFrame.setVisible(true);
        for (String arg : mCommandLineArgs.getPaths()) {
            mMainFrame.openModel(new File(arg));
        }

        try {
            GarudaClient.start(mMainFrame);
        } catch (GarudaConnectionNotInitializedException | NetworkException e) {
            Logger.getRootLogger().info("Garuda client fails at launch", e);
        }
    }

    public MainFrame getMainFrame () {
        return mMainFrame;
    }
}
