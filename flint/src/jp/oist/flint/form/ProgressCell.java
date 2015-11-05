/* -*- Mode: Java; tab-width: 4; indent-tabs-mode: nil; c-basic-offset: 4 -*- vim:set ts=4 sw=4 sts=4 et: */
package jp.oist.flint.form;

import jp.oist.flint.dao.SimulationDao;
import jp.oist.flint.desktop.CancelSimulationActionListener;
import jp.oist.flint.desktop.Document;
import jp.oist.flint.executor.PhspSimulator;
import jp.oist.flint.form.util.ComponentFactory;
import jp.oist.flint.textformula.analyzer.ParseException;
import java.awt.Color;
import java.awt.Dimension;
import java.awt.FlowLayout;
import java.awt.GridLayout;
import java.awt.event.ActionListener;
import java.io.File;
import javax.swing.Box;
import javax.swing.BoxLayout;
import javax.swing.ImageIcon;
import javax.swing.JButton;
import javax.swing.JPanel;
import javax.swing.JProgressBar;
import javax.swing.UIManager;
import javax.swing.border.EtchedBorder;
import javax.swing.border.TitledBorder;
import javax.xml.parsers.ParserConfigurationException;
import javax.xml.transform.TransformerException;

public class ProgressCell extends JPanel {

    private final Document mDocument;

    private final JButton mJobBtn;

    private final JButton mCancelBtn;

    private final JProgressBar mProgressBar;

    private CancelSimulationActionListener mCancelActionListener = null;

    private ActionListener mDetailActionListener = null;

    public ProgressCell(Document document) {
        mDocument = document;
        File file = document.getFile();
        String title = file.getName();
        setToolTipText(String.format("%s [%s]", title, file.getPath()));

        setLayout(new GridLayout(2,1));
        setBorder(new TitledBorder(new EtchedBorder(), title));

        // fixed dimension
        setMinimumSize(new Dimension(140, 75));
        setMaximumSize(new Dimension(140, 75));
        setPreferredSize(new Dimension(140, 75));

        mProgressBar = new JProgressBar();
        mJobBtn = ComponentFactory.createSquareButton("Detail", "progress.plot", new Dimension(48,20));
        mJobBtn.setEnabled(false);

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

    /*
     * Change the foreground/background colors like JList components.
     */
    public void setSelected(boolean selected) {
        Color bc;
        Color fc;
        if (selected) {
            bc = UIManager.getColor("List.selectionBackground");
            fc = UIManager.getColor("List.selectionForeground");
        } else {
            bc = UIManager.getColor("List.background");
            fc = UIManager.getColor("List.foreground");
        }
        setBackground(bc);
        setForeground(fc);
        repaint();
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

    public synchronized void setText (String txt) {
        TitledBorder titledBorder = (TitledBorder)getBorder();
        titledBorder.setTitle(txt);
    }

    public synchronized String getText () {
        TitledBorder titledBorder = (TitledBorder)getBorder();
        return titledBorder.getTitle();
    }

    public Document getDocument() {
        return mDocument;
    }

    public int getStatusBarProgress () {
        return mProgressBar.getValue();
    }

    public void clear() {
        mCancelBtn.setEnabled(false);
        mJobBtn.setEnabled(false);
    }

    public void setProgress(String msg, int value) {
        mProgressBar.setString(msg);
        mProgressBar.setValue(value);
        mProgressBar.repaint();
    }

    public void progressStarted(SimulationDao simulationDao) {
        String msg = "preparing...";

        mProgressBar.setValue(0);
        mProgressBar.setString(msg);

        mCancelActionListener = new CancelSimulationActionListener(simulationDao, this);
        mCancelBtn.addActionListener(mCancelActionListener);
        mCancelBtn.setEnabled(true);
    }

    public void progressFinished(String msg, int value) {
        setProgress(msg, value);

        mCancelBtn.setEnabled(false);
        mCancelBtn.removeActionListener(mCancelActionListener);
        mCancelActionListener = null;
    }

    /*
     * Supposed to be called in EDT.
     */
    public boolean prepareWindow(PhspSimulator simulator, String path, int n)
        throws ParseException, ParserConfigurationException, TransformerException {
        if (!mDocument.getFile().getPath().equals(path))
            return false;
        if (mDetailActionListener != null)
            mJobBtn.removeActionListener(mDetailActionListener);
        if (n < 10) {
            mDetailActionListener = new ProgressDetailActionListener(simulator, mDocument.getSubFrame());
        } else {
            mDetailActionListener = new TaskDetailActionListener(simulator, mDocument.getSubFrame());
        }
        mJobBtn.addActionListener(mDetailActionListener);
        mJobBtn.setEnabled(true);
        return true;
    }
}
