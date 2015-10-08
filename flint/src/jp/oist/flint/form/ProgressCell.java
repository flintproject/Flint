/* -*- Mode: Java; tab-width: 4; indent-tabs-mode: nil; c-basic-offset: 4 -*- vim:set ts=4 sw=4 sts=4 et: */
package jp.oist.flint.form;

import jp.oist.flint.desktop.CancelSimulationActionListener;
import jp.oist.flint.desktop.Document;
import static jp.oist.flint.form.ProgressPane.JOB_ACTION_COMMAND;
import jp.oist.flint.form.util.ComponentFactory;
import java.awt.Dimension;
import java.awt.FlowLayout;
import java.awt.GridLayout;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.beans.PropertyChangeListener;
import java.io.File;
import java.util.ArrayList;
import java.util.HashMap;
import javax.swing.Box;
import javax.swing.BoxLayout;
import javax.swing.DefaultListModel;
import javax.swing.ImageIcon;
import javax.swing.JButton;
import javax.swing.JList;
import javax.swing.JPanel;
import javax.swing.JProgressBar;
import javax.swing.border.EtchedBorder;
import javax.swing.border.TitledBorder;

public class ProgressCell extends JPanel implements ActionListener {

    public final static String IS_SELECTED_PROPERTY = "selected";

    private final Document mDocument;

    private final JList mParent;

    private JButton mJobBtn = null;

    private JButton mCancelBtn = null;

    private JProgressBar mProgressBar = null;

    private boolean mIsSelected = false;

    private final HashMap<String, Object> mVars =
        new HashMap<>();

    private final ArrayList<ActionListener> mActionListeners;

    private final ArrayList<PropertyChangeListener> mPropertyChangeListeners;

    public ProgressCell(Document document, JList parent) {
        mDocument = document;
        mParent = parent;
        mPropertyChangeListeners = new ArrayList<>();
        mActionListeners = new ArrayList<>();
        File file = document.getFile();
        String title = file.getName();
        setToolTipText(String.format("%s [%s]", title, file.getPath()));
        initComponents(title);
    }

    private void initComponents(String title) {
        setLayout(new GridLayout(2,1));
        setOpaque(true);
        setBorder(new TitledBorder(new EtchedBorder(), title));

        setPreferredSize(new Dimension(140, 75));

        mProgressBar = new JProgressBar();
        mJobBtn = ComponentFactory.createSquareButton("Detail", "progress.plot", new Dimension(48,20));
        mJobBtn.setActionCommand(JOB_ACTION_COMMAND);
        mJobBtn.addActionListener(this);
        mJobBtn.putClientProperty("owner", this);

        JPanel upperPane = new JPanel();
        upperPane.setLayout(new BoxLayout(upperPane, BoxLayout.LINE_AXIS));
        upperPane.add(Box.createRigidArea(new Dimension(5,0)));

        JPanel bottomPane = new JPanel(new FlowLayout(FlowLayout.RIGHT));

        bottomPane.setPreferredSize(new Dimension(10,10));
        ((FlowLayout)bottomPane.getLayout()).setVgap(0);
        ((FlowLayout)bottomPane.getLayout()).setHgap(0);

        upperPane.setOpaque(false);
        bottomPane.setOpaque(false);

        // progress bar
        Dimension progressSize = new Dimension(new Dimension(80, 20));
        mProgressBar.setMaximumSize(new Dimension(Short.MAX_VALUE, 20));
        mProgressBar.setMinimumSize(progressSize);
        mProgressBar.setPreferredSize(progressSize);
        mProgressBar.setSize(progressSize);
        mProgressBar.setBorderPainted(true);
        mProgressBar.setStringPainted(true);
        mProgressBar.setString("idle");

        int spaceW = 1; int spaceH = 20;

        upperPane.add(createSpacePanel(new Dimension(spaceW, spaceH)));
        upperPane.add(mProgressBar);
        upperPane.add(createSpacePanel(new Dimension(spaceW, spaceH)));

        mCancelBtn  = new JButton();
        mCancelBtn.addActionListener(new CancelSimulationActionListener(this));
        mCancelBtn.setIcon(new ImageIcon(getClass().getResource("/jp/oist/flint/image/cancel.png")));
        Dimension btnSize = new Dimension(20,20);
        mCancelBtn.setSize(btnSize);
        mCancelBtn.setPreferredSize(btnSize);
        mCancelBtn.setMaximumSize(btnSize);
        mCancelBtn.setMinimumSize(btnSize);
        mCancelBtn.setOpaque(false);
        mCancelBtn.setEnabled(false);
        mCancelBtn.putClientProperty("owner", this);
        upperPane.add(mCancelBtn);

        upperPane.add(createSpacePanel(new Dimension(spaceW, spaceH)));

        add(upperPane);

        FlowLayout buttonLayout = new FlowLayout(FlowLayout.RIGHT);
        JPanel buttonPane = new JPanel(buttonLayout);
        buttonPane.setOpaque(false);
        buttonLayout.setHgap(1);
        buttonLayout.setVgap(3);
        buttonPane.add(mJobBtn);

        // plot button
        bottomPane.setPreferredSize(new Dimension(120, 25));
        bottomPane.add(buttonPane);

        add(bottomPane);
    }

    private JPanel createSpacePanel (Dimension fixedSize) {
        JPanel panel = new JPanel();
        panel.setSize(fixedSize);
        panel.setPreferredSize(fixedSize);
        panel.setMaximumSize(fixedSize);
        panel.setMinimumSize(fixedSize);

        panel.setRequestFocusEnabled(false);
        panel.setOpaque(false);

        return panel;
    }

    public void setGeneralButtonEnabled (boolean b) {
        mJobBtn.setEnabled(b);
        repaint();
    }

    public synchronized void setText (String txt) {
        TitledBorder titledBorder = (TitledBorder)getBorder();
        titledBorder.setTitle(txt);
    }

    public synchronized String getText () {
        TitledBorder titledBorder = (TitledBorder)getBorder();
        return titledBorder.getTitle();
    }

    public void setSelected (boolean isSelected) {
        if (mIsSelected == isSelected)
            return;

        Boolean oldValue = mIsSelected;
        Boolean newValue = isSelected;
        firePropertyChange(IS_SELECTED_PROPERTY, oldValue, newValue);

        mIsSelected = isSelected;

        repaint();
    }

    public boolean isSelected () {
        return mIsSelected;
    }

    public Document getDocument() {
        return mDocument;
    }

    public int getStatusBarProgress () {
        return mProgressBar.getValue();
    }

    public void setProgress(String msg, int minimum, int maximum, int value) {
        mProgressBar.setString(msg);
        mProgressBar.setValue(value);

        DefaultListModel model = (DefaultListModel)mParent.getModel();
        int myIndex = model.indexOf(this);

        // trigger to repaint the ProgressCell
        mParent.repaint(mParent.getCellBounds(myIndex, myIndex));
    }

    public void progressStarted () {
        String msg = "preparing...";

        mProgressBar.setValue(0);
        mProgressBar.setString(msg);
        setGeneralButtonEnabled(true);
        mCancelBtn.setEnabled(true);
    }

    public void progressFinished(String msg, int minimum, int maximum, int value) {
        setProgress(msg, minimum, maximum, value);
        mCancelBtn.setEnabled(false);
    }

    public void addActionListener (ActionListener l) {
        mActionListeners.add(l);
    }

    public void removeActionListener (ActionListener l) {
        mActionListeners.remove(l);
    }

    @Override
    public void actionPerformed(ActionEvent evt) {
        fireActionPerformed(evt);
    }

    protected void fireActionPerformed (ActionEvent evt) {
        for (ActionListener l : mActionListeners)
            l.actionPerformed(evt);
    }
}
