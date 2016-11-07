/* -*- Mode: Java; tab-width: 4; indent-tabs-mode: nil; c-basic-offset: 4 -*- vim:set ts=4 sw=4 sts=4 et: */
package jp.oist.flint.form;

import jp.oist.flint.command.ISessionListener;
import jp.oist.flint.desktop.Document;
import jp.oist.flint.desktop.IDesktopListener;
import jp.oist.flint.desktop.ILoadingListener;
import jp.oist.flint.desktop.ISimulationListener;
import jp.oist.flint.executor.PhspSimulator;
import java.awt.Toolkit;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.awt.event.KeyEvent;
import java.io.File;
import java.util.ArrayList;
import javax.swing.JMenu;
import javax.swing.JMenuBar;
import javax.swing.JMenuItem;
import javax.swing.KeyStroke;

public class MenuBar extends JMenuBar
    implements IDesktopListener, ILoadingListener,
               ISessionListener, ISimulationListener {

    /* Item names on Menu "File" */
    public final static String OPEN = "menu.file.open";
    public final static String RECENT_MODELS = "menu.file.recentModels";
    public final static String EXPORT = "menu.file.export";
    public final static String EXPORT_C = "menu.file.export.c";
    public final static String CLOSE = "menu.file.close";
    public final static String LOAD_CONFIGURATION = "menu.file.loadConfiguration";
    public final static String SAVE_CONFIGURATION = "menu.file.saveConfiguration";
    public final static String SAVE_AS_PHSP = "menu.file.saveAsPhsp";
    public final static String EXIT = "menu.file.exit";

    /* Item names on Menu "Edit" */
    public final static String COPY = "menu.edit.copy";
    public final static String CUT = "menu.edit.cut";
    public final static String PREFERENCE = "menu.edit.preference";

    /* Item names on Menu "Control" */
    public final static String RUN = "menu.control.run";
    public final static String PAUSE = "menu.control.pause";
    public final static String RESUME = "menu.control.resume";
    public final static String SEND_TO_FLINT_K3 = "menu.control.sendToK3";

    /* Item names on Menu "Help" */
    public final static String ABOUT = "menu.help.about";

    /* Items on Menu "File" */
    private JMenuItem mItemOpen;
    private JMenu     mMenuRecentModels;
    private JMenu     mMenuExport = null;
    private JMenuItem mItemExportC = null;
    private JMenuItem mItemClose;
    private JMenuItem mItemLoadConfiguration;
    private JMenuItem mItemSaveConfiguration;
    private JMenuItem mItemSaveAsPhsp;
    private JMenuItem mItemExit;

    /* Items on Menu "Edit" */
    private JMenuItem mItemCopy;
    private JMenuItem mItemCut;
    private JMenuItem mItemPreference;

    /* Items on Menu "Control" */
    private JMenuItem mItemRun;
    private JMenuItem mItemPause;
    private JMenuItem mItemResume;
    private JMenuItem mItemSendToFlintK3;

    /* Items on Menu "Help */
    private JMenuItem mItemAbout;

    private final MainFrame mFrame;

    private String mLastPath = null;

    public MenuBar(MainFrame frame) {
        super();

        assert frame != null;
        mFrame = frame;
        initComponents();
    }

    private void initComponents () {
        // create Menu "File"
        JMenu menuFile = new JMenu("File");
        menuFile.setName("menu.file");

        mItemOpen = new JMenuItem("Open");
        mItemOpen.setName(OPEN);
        mItemOpen.setAccelerator(KeyStroke.getKeyStroke(KeyEvent.VK_O, 
                Toolkit.getDefaultToolkit().getMenuShortcutKeyMask()));
        mItemOpen.setToolTipText("Open an existing file");
        mItemOpen.addActionListener(new ActionListener () {
            @Override
            public void actionPerformed(ActionEvent e) {
                mFrame.openPerformed(mLastPath);
            }
        });

        mMenuRecentModels = new JMenu("Recent Models...");
        mMenuRecentModels.setName(RECENT_MODELS);

        mMenuExport = new JMenu("Export...");
        mMenuExport.setName(EXPORT);
        mMenuExport.setEnabled(false);

        mItemExportC = new JMenuItem("C");
        mItemExportC.setName(EXPORT_C);
        mItemExportC.setToolTipText("Export simulation code into C");
        mItemExportC.addActionListener(new ActionListener() {
                @Override
                public void actionPerformed(ActionEvent e) {
                    mFrame.exportIntoC();
                }
            });
        mMenuExport.add(mItemExportC);

        mItemClose        = new JMenuItem("Close");
        mItemClose.setName(CLOSE);
        mItemClose.setEnabled(false);
        mItemClose.setAccelerator(KeyStroke.getKeyStroke(
                KeyEvent.VK_W, Toolkit.getDefaultToolkit().getMenuShortcutKeyMask()));
        mItemClose.setToolTipText("Close model");
        mItemClose.addActionListener(new ActionListener () {
            @Override
            public void actionPerformed(ActionEvent e) {
                mFrame.closePerformed(e.getSource());
            }
        });

        mItemLoadConfiguration = new JMenuItem("Load Configuration");
        mItemLoadConfiguration.setName(LOAD_CONFIGURATION);
        mItemLoadConfiguration.setEnabled(false);
        mItemLoadConfiguration.addActionListener(new ActionListener() {
            @Override
            public void actionPerformed(ActionEvent e) {
                mFrame.loadConfigurationPerformed(e.getSource());
            }
        });

        mItemSaveConfiguration = new JMenuItem("Save Configuration");
        mItemSaveConfiguration.setName(SAVE_CONFIGURATION);
        mItemSaveConfiguration.setEnabled(false);
        mItemSaveConfiguration.addActionListener(new ActionListener() {
            @Override
            public void actionPerformed(ActionEvent e) {
                mFrame.saveConfigurationPerformed(e.getSource());
            }
        });

        mItemSaveAsPhsp = new JMenuItem("Save As PHSP");
        mItemSaveAsPhsp.setName(SAVE_AS_PHSP);
        mItemSaveAsPhsp.setEnabled(false);
        mItemSaveAsPhsp.addActionListener(new ActionListener(){
            @Override
            public void actionPerformed(ActionEvent e) {
                mFrame.saveAsPhspPerformed(e.getSource());
            }
        });

        mItemExit = new JMenuItem("Exit");
        mItemExit.setName(EXIT);
        mItemExit.setAccelerator(javax.swing.KeyStroke.getKeyStroke(java.awt.event.KeyEvent.VK_Q, 
                Toolkit.getDefaultToolkit().getMenuShortcutKeyMask()));
        mItemExit.setToolTipText("Exit the application");
        mItemExit.addActionListener(new ActionListener () {
            @Override
            public void actionPerformed(ActionEvent e) {
                mFrame.exitPerformed(e.getSource());
            }
        });

        menuFile.add(mItemOpen);
        menuFile.add(mMenuRecentModels);
        if (mMenuExport != null)
            menuFile.add(mMenuExport);
        menuFile.add(mItemClose);
        menuFile.addSeparator();
        menuFile.add(mItemLoadConfiguration);
        menuFile.add(mItemSaveConfiguration);
        menuFile.addSeparator();
        menuFile.add(mItemSaveAsPhsp);
        menuFile.addSeparator();
        menuFile.add(mItemExit);

        // create Menu "Edit"
        JMenu menuEdit = new JMenu("Edit"); 
        menuEdit.setName("menu.edit");

        mItemCopy = new JMenuItem("Copy");
        mItemCopy.setName(COPY);
        mItemCopy.setEnabled(false);
        mItemCopy.setAccelerator(KeyStroke.getKeyStroke(
                KeyEvent.VK_C, Toolkit.getDefaultToolkit().getMenuShortcutKeyMask()));
        mItemCopy.setToolTipText("Copy the log text");
        mItemCopy.addActionListener(new ActionListener() {
            @Override
            public void actionPerformed(ActionEvent e) {
                mFrame.copyPerformed(e.getSource());
            }
        });

        mItemCut  = new JMenuItem("Cut");
        mItemCut.setName(CUT);
        mItemCut.setEnabled(false);
        mItemCut.setAccelerator(KeyStroke.getKeyStroke(
                KeyEvent.VK_X, Toolkit.getDefaultToolkit().getMenuShortcutKeyMask()));
        mItemCut.setToolTipText("Cut the log text");
        mItemCut.addActionListener(new ActionListener(){
            @Override
            public void actionPerformed(ActionEvent e) {
                mFrame.cutPerformed(e.getSource());
            }
        });

        mItemPreference = new JMenuItem("Preference");
        mItemPreference.setName(PREFERENCE);
        mItemPreference.setAccelerator(KeyStroke.getKeyStroke(
                KeyEvent.VK_COMMA, Toolkit.getDefaultToolkit().getMenuShortcutKeyMask()));
        mItemPreference.addActionListener(new ActionListener (){
            @Override
            public void actionPerformed(ActionEvent e) {
                mFrame.preferencePerformed(e.getSource());
            }
        });

        menuEdit.add(mItemCopy);
        menuEdit.add(mItemCut);
        menuEdit.addSeparator();
        menuEdit.add(mItemPreference);

        // create Menu "Control"
        JMenu menuControl = new JMenu("Control");
        menuControl.setName("menu.control");

        mItemRun = new JMenuItem("Run");
        mItemRun.setName(RUN);
        mItemRun.setEnabled(false);
        mItemRun.setAccelerator(KeyStroke.getKeyStroke(
                KeyEvent.VK_R, KeyEvent.ALT_DOWN_MASK));
        mItemRun.addActionListener(new ActionListener () {
            @Override
            public void actionPerformed(ActionEvent e) {
                mFrame.simulationRunPerformed(e.getSource());
            }
        });

        mItemPause = new JMenuItem("Pause");
        mItemPause.setName(PAUSE);
        mItemPause.setEnabled(false);
        mItemPause.setAccelerator(KeyStroke.getKeyStroke(
                KeyEvent.VK_P, KeyEvent.ALT_DOWN_MASK));
        mItemPause.addActionListener(new ActionListener () {
            @Override
            public void actionPerformed(ActionEvent e) {
                mFrame.simulationPausePerformed(e.getSource());
            }
        });

        mItemResume = new JMenuItem("Resume");
        mItemResume.setName(RESUME);
        mItemResume.setEnabled(false);
        mItemResume.setAccelerator(KeyStroke.getKeyStroke(
                KeyEvent.VK_S, KeyEvent.ALT_DOWN_MASK));
        mItemResume.addActionListener(new ActionListener () {
            @Override
            public void actionPerformed(ActionEvent e) {
                mFrame.simulationResumePerformed(e.getSource());
            }
        });

        mItemSendToFlintK3 = new JMenuItem("Send to Flint K3");
        mItemSendToFlintK3.setName(SEND_TO_FLINT_K3);
        mItemSendToFlintK3.addActionListener(new ActionListener(){
            @Override
            public void actionPerformed(ActionEvent e) {
                mFrame.sendToK3Performed(e.getSource());
            }
        });
        mItemSendToFlintK3.setEnabled(false);

        menuControl.add(mItemRun);
        menuControl.add(mItemPause);
        menuControl.add(mItemResume);
        menuControl.add(mItemSendToFlintK3);

        // create Menu "Help"
        JMenu menuHelp = new JMenu("Help");
        menuHelp.setName("menu.help");

        mItemAbout = new JMenuItem("About...");
        mItemAbout.setName(ABOUT);
        mItemAbout.setToolTipText("About this application");
        mItemAbout.addActionListener(new ActionListener () {
            @Override
            public void actionPerformed(ActionEvent e) {
                mFrame.aboutPerformed(e.getSource());
            }
        });

        menuHelp.add(mItemAbout);

        add(menuFile);
        add(menuEdit);
        add(menuControl);
        add(menuHelp);
    }

    private JMenuItem obtainRecentModelMenuItem(final File model) {
        JMenuItem m = new JMenuItem(model.getName());
        m.setToolTipText(model.getPath());
        m.addActionListener(new ActionListener() {
            @Override
            public void actionPerformed(ActionEvent e) {
                mFrame.recentModelPerformed(e.getSource(), model);
            }
        });
        return m;
    }

    @Override
    public void recentModelsUpdated(final ArrayList<File> recentModels) {
        mMenuRecentModels.removeAll();
        for (File model : recentModels) {
            mMenuRecentModels.add(obtainRecentModelMenuItem(model));
        }

        // remember the parent directory of the last
        if (!recentModels.isEmpty())
            mLastPath = recentModels.get(0).getParent();
    }

    /* ILoadingListener */

    @Override
    public void loadingStarted() {
        if (mMenuExport != null)
            mMenuExport.setEnabled(false);
        mItemRun.setEnabled(false);
        mItemSendToFlintK3.setEnabled(false);
    }

    @Override
    public void loadingDone() {
        if (mMenuExport != null)
            mMenuExport.setEnabled(true);
        mItemRun.setEnabled(true);
        mItemSendToFlintK3.setEnabled(true);
    }

    /* ISimulationListener */

    @Override
    public void simulationRequested() {
        mItemOpen.setEnabled(false);
        mMenuRecentModels.setEnabled(false);
        if (mMenuExport != null)
            mMenuExport.setEnabled(false);
        mItemClose.setEnabled(false);
        mItemLoadConfiguration.setEnabled(false);
        mItemSaveConfiguration.setEnabled(false);
        mItemSaveAsPhsp.setEnabled(false);
        mItemRun.setEnabled(false);
        mItemPause.setEnabled(true);
        mItemResume.setEnabled(false);
        mItemSendToFlintK3.setEnabled(false);
    }

    @Override
    public void simulationStarted(PhspSimulator simulator) {
    }

    @Override
    public void simulationDone() {
        mItemOpen.setEnabled(true);
        mMenuRecentModels.setEnabled(true);
        if (mMenuExport != null)
            mMenuExport.setEnabled(true);
        mItemClose.setEnabled(true);
        mItemLoadConfiguration.setEnabled(true);
        mItemSaveConfiguration.setEnabled(true);
        mItemSaveAsPhsp.setEnabled(true);
        mItemRun.setEnabled(true);
        mItemPause.setEnabled(false);
        mItemResume.setEnabled(false);
        mItemSendToFlintK3.setEnabled(true);
    }

    @Override
    public void simulationPaused() {
        mItemPause.setEnabled(false);
        mItemResume.setEnabled(true);
    }

    @Override
    public void simulationResumed() {
        mItemPause.setEnabled(true);
        mItemResume.setEnabled(false);
    }

    /* IDesktopListener */

    @Override
    public void documentAdded(Document doc) {
        if (mMenuExport != null)
            mMenuExport.setEnabled(true);
        mItemSaveAsPhsp.setEnabled(true);
        mItemLoadConfiguration.setEnabled(true);
        mItemSaveConfiguration.setEnabled(true);
        mItemRun.setEnabled(true);
        mItemSendToFlintK3.setEnabled(true);
        mItemClose.setEnabled(true);
        mItemCopy.setEnabled(true);
        mItemCut.setEnabled(true);
    }

    @Override
    public void documentRemoved(Document doc, boolean empty) {
        if (empty) {
            if (mMenuExport != null)
                mMenuExport.setEnabled(false);
            mItemSaveAsPhsp.setEnabled(false);
            mItemLoadConfiguration.setEnabled(false);
            mItemSaveConfiguration.setEnabled(false);
            mItemRun.setEnabled(false);
            mItemSendToFlintK3.setEnabled(false);
            mItemClose.setEnabled(false);
            mItemCopy.setEnabled(false);
            mItemCut.setEnabled(false);
        }
    }
}
