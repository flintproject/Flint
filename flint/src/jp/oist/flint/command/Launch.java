/* -*- Mode: Java; tab-width: 4; indent-tabs-mode: nil; c-basic-offset: 4 -*- vim:set ts=4 sw=4 sts=4 et: */
package jp.oist.flint.command;

import jp.oist.flint.desktop.Desktop;
import jp.oist.flint.desktop.SessionHandler;
import jp.oist.flint.form.ControlPane;
import jp.oist.flint.form.MainFrame;
import jp.oist.flint.form.MenuBar;
import jp.oist.flint.form.ProgressPane;
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

    private MainFrame mMainFrame;

    public Launch(final CommandLineArguments commandLineArgs) {
        mCommandLineArgs = commandLineArgs;
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

        Session session = new Session();
        MenuBar menuBar = new MenuBar();
        session.addListener(menuBar);
        ControlPane controlPane = new ControlPane();
        ProgressPane progressPane = new ProgressPane();
        try {
            Desktop desktop = new Desktop(progressPane);
            desktop.addListener(new SessionHandler(session));
            desktop.addListener(menuBar);
            desktop.addListener(controlPane);
            desktop.addListener(progressPane);
            mMainFrame = new MainFrame(desktop, controlPane, progressPane);
            mMainFrame.setJMenuBar(menuBar);
            menuBar.setDelegator(mMainFrame);
            controlPane.setDelegator(mMainFrame);
            desktop.addListener(mMainFrame);
            desktop.addLoadingListener(controlPane);
            desktop.addLoadingListener(menuBar);
            desktop.addSimulationListener(controlPane);
            desktop.addSimulationListener(menuBar);
            desktop.addSimulationListener(progressPane);
            desktop.startWatching();
        } catch (IOException ex) {
            Logger.getRootLogger().fatal("could not launch " + MainFrame.class.getName() + ": " + ex.getMessage());
            System.exit(1);
            return;
        }

        Runtime.getRuntime().addShutdownHook(new Rescue(session));

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
