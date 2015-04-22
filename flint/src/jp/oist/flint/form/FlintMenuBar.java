/* -*- Mode: Java; tab-width: 4; indent-tabs-mode: nil; c-basic-offset: 4 -*- vim:set ts=4 sw=4 sts=4 et: */

package jp.oist.flint.form;

import jp.oist.flint.executor.PhspSimulator;

import java.awt.Toolkit;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.awt.event.KeyEvent;
import java.io.File;
import java.util.List;
import javax.swing.JMenu;
import javax.swing.JMenuBar;
import javax.swing.JMenuItem;
import javax.swing.KeyStroke;

public class FlintMenuBar extends JMenuBar 
    implements MainFrame.Listener, PhspSimulator.Listener {

    /* Item names on Menu "File" */
    public final static String OPEN = "menu.file.open";
    public final static String RECENT_MODELS = "menu.file.recentModels";
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
    public final static String SEND_TO_FLINT_K3 = "menu.control.sendToK3";

    /* Item names on Menu "Help" */
    public final static String ABOUT = "menu.help.about";

    private final static FlintMenuBar instance = new FlintMenuBar();

    public static FlintMenuBar getInstance () {
        return instance;
    }

    /* Items on Menu "File" */
    private JMenuItem mItemOpen;
    private JMenu     mMenuRecentModels;
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
    private JMenuItem mItemSendToFlintK3;

    /* Items on Menu "Window" */
    private JMenuItem mItemProgresses;
    private JMenuItem mItemControlPanel;
    private JMenuItem mItemNextTab;
    private JMenuItem mItemPreviousTab;

    /* Items on Menu "Help */
    private JMenuItem mItemAbout;

    private IFlintMenuBarDelegator mDelegator;

    protected FlintMenuBar () {
        super();

        mDelegator = null;
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
                if (mDelegator != null) 
                    mDelegator.openPerformed(e.getSource());
            }
        });

        mMenuRecentModels = new JMenu("Recent Models...");
        mMenuRecentModels.setName(RECENT_MODELS);

        mItemClose        = new JMenuItem("Close");
        mItemClose.setName(CLOSE);
        mItemClose.setEnabled(false);
        mItemClose.setAccelerator(KeyStroke.getKeyStroke(
                KeyEvent.VK_W, Toolkit.getDefaultToolkit().getMenuShortcutKeyMask()));
        mItemClose.setToolTipText("Close model");
        mItemClose.addActionListener(new ActionListener () {
            @Override
            public void actionPerformed(ActionEvent e) {
                if (mDelegator != null)  {
                    mDelegator.closePerformed(e.getSource());
                }
            }
        });

        mItemLoadConfiguration = new JMenuItem("Load Configuration");
        mItemLoadConfiguration.setName(LOAD_CONFIGURATION);
        mItemLoadConfiguration.setEnabled(false);
        mItemLoadConfiguration.addActionListener(new ActionListener() {
            @Override
            public void actionPerformed(ActionEvent e) {
                if (mDelegator != null) 
                    mDelegator.loadConfigurationPerformed(e.getSource());
            }
        });

        mItemSaveConfiguration = new JMenuItem("Save Configuration");
        mItemSaveConfiguration.setName(SAVE_CONFIGURATION);
        mItemSaveConfiguration.setEnabled(false);
        mItemSaveConfiguration.addActionListener(new ActionListener() {
            @Override
            public void actionPerformed(ActionEvent e) {
                if (mDelegator != null)                 
                    mDelegator.saveConfigurationPerformed(e.getSource());
            }
        });

        mItemSaveAsPhsp = new JMenuItem("Save As PHSP");
        mItemSaveAsPhsp.setName(SAVE_AS_PHSP);
        mItemSaveAsPhsp.setEnabled(false);
        mItemSaveAsPhsp.addActionListener(new ActionListener(){
            @Override
            public void actionPerformed(ActionEvent e) {
                if (mDelegator != null)
                    mDelegator.saveAsPhspPerformed(e.getSource());
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
                if (mDelegator != null)
                    mDelegator.exitPerformed(e.getSource());
            }
        });

        menuFile.add(mItemOpen);
        menuFile.add(mMenuRecentModels);
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
                if (mDelegator != null)
                    mDelegator.copyPerformed(e.getSource());
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
                if (mDelegator != null)
                    mDelegator.cutPerformed(e.getSource());
            }
        });

        mItemPreference = new JMenuItem("Preference");
        mItemPreference.setName(PREFERENCE);
        mItemPreference.setAccelerator(KeyStroke.getKeyStroke(
                KeyEvent.VK_COMMA, Toolkit.getDefaultToolkit().getMenuShortcutKeyMask()));
        mItemPreference.addActionListener(new ActionListener (){
            @Override
            public void actionPerformed(ActionEvent e) {
                if (mDelegator != null)
                    mDelegator.preferencePerformed(e.getSource());
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
                if (mDelegator != null)
                    mDelegator.simulationRunPerformed(e.getSource());
            }
        });

        mItemSendToFlintK3 = new JMenuItem("Send to Flint K3");
        mItemSendToFlintK3.setName(SEND_TO_FLINT_K3);
        mItemSendToFlintK3.addActionListener(new ActionListener(){
            @Override
            public void actionPerformed(ActionEvent e) {
                if (mDelegator != null)
                    mDelegator.sendToK3Performed(e.getSource());
            }
        });
        mItemSendToFlintK3.setEnabled(false);

        menuControl.add(mItemRun);
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
                if (mDelegator != null)
                    mDelegator.aboutPerformed(e.getSource());
            }
        });

        menuHelp.add(mItemAbout);

        add(menuFile);
        add(menuEdit);
        add(menuControl);
        add(menuHelp);
    }

    public void setDelegator (IFlintMenuBarDelegator delegator) {
        mDelegator = delegator;
    }

    public IFlintMenuBarDelegator getDelegator () {
        return mDelegator;
    }

    private JMenuItem findJMenuItemByName (String name) {
        int menuCount = getMenuCount();
        for (int i=0; i<menuCount; i++) {
            JMenu menu = getMenu(i);
            JMenuItem item = findJMenuItemByName(menu, name);
            if (item != null) return item;
        }
        return null;
    }

    private JMenuItem findJMenuItemByName (JMenu parent, String name) {
        int itemCount = parent.getItemCount();
        for (int i=0; i<itemCount; i++) {
            JMenuItem item = parent.getItem(i);
            if (item == null) continue;

            if (name.equals(item.getName()))
                return item;

            if (item instanceof JMenu)
                findJMenuItemByName((JMenu)item, name);
        }
        return null;
    }

    private JMenuItem obtainRecentModelModelItem (final File model) {
        JMenuItem m = new JMenuItem(model.getName());
        m.setToolTipText(model.getPath());
        m.addActionListener(new ActionListener() {
            @Override
            public void actionPerformed(ActionEvent e) {
                if (mDelegator != null)
                    mDelegator.recentModelPerformed(e.getSource(), model);
            }
        });
        return m;
    }

    public void addRecentModel (File model) {
        mMenuRecentModels.add(obtainRecentModelModelItem(model));
    }

    public void removeRecentModel (File model) {
        JMenu menu = mMenuRecentModels;
        int itemCount = menu.getItemCount();
        JMenuItem target = null;
        for (int i=0; i<itemCount; i++) {
            JMenuItem item = menu.getItem(i);
            if (item == null)
                continue;

            if (item.getToolTipText().equals(model.getPath())) {
                target = item;
                break;
            }
        }

        if (target != null)
            menu.remove(target);
    } 

    public void removeAllRecentModels () {
        JMenu menu = mMenuRecentModels;
        menu.removeAll();
    }

    public void fireUpdateRecentModels (List<File> modelFiles) {
        if (mDelegator != null) {
            JMenuItem item = mMenuRecentModels;
            mDelegator.updateRecentModels(item, modelFiles);
        }
    }

    public void setMenuItemEnabled (String name, boolean enabled) {
        if (name == null || name.isEmpty())
            return;

        JMenuItem item = findJMenuItemByName(name);
        if (item != null)
            item.setEnabled(enabled);
    }

    /*
     * Implements PhspSimulator.Listener
     */
    @Override
    public void onSimulationStarted (PhspSimulator.Event evt) {
        mItemOpen.setEnabled(false);
        mMenuRecentModels.setEnabled(false);
        mItemClose.setEnabled(false);
        mItemLoadConfiguration.setEnabled(false);
        mItemSaveConfiguration.setEnabled(false);
        mItemSaveAsPhsp.setEnabled(false);
        mItemRun.setEnabled(false);
        mItemSendToFlintK3.setEnabled(false);
    }

    @Override
    public void onSimulationExited (PhspSimulator.Event evt) {
        mItemOpen.setEnabled(true);
        mMenuRecentModels.setEnabled(true);
        mItemClose.setEnabled(true);
        mItemLoadConfiguration.setEnabled(true);
        mItemSaveConfiguration.setEnabled(true);
        mItemSaveAsPhsp.setEnabled(true);
        mItemRun.setEnabled(true);
        mItemSendToFlintK3.setEnabled(true);
    }

    /*
     * Implements MainFrame.Listener
     */
    @Override
    public void onModelOpened(MainFrame.Event evt) {
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
    public void onModelClosed(MainFrame.Event evt) {
        MainFrame mainFrame = (MainFrame)evt.getSource();
        int length = mainFrame.getModelLength();

        if (length <= 0) {
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
