/* -*- Mode: Java; tab-width: 4; indent-tabs-mode: nil; c-basic-offset: 4 -*- vim:set ts=4 sw=4 sts=4 et: */
package jp.oist.flint.form;

import jp.oist.flint.desktop.Document;
import jp.oist.flint.desktop.IDesktopListener;
import jp.oist.flint.desktop.ILoadingListener;
import jp.oist.flint.desktop.ISimulationListener;
import jp.oist.flint.executor.PhspSimulator;
import java.awt.BorderLayout;
import java.awt.Container;
import java.awt.Dimension;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.awt.event.FocusEvent;
import java.awt.event.FocusListener;
import javax.swing.BorderFactory;
import javax.swing.JPanel;
import javax.swing.JButton;

public class ControlPane extends PeripheralPane
    implements FocusListener, IDesktopListener,
               ILoadingListener, ISimulationListener {

    public final static String RUN = "controlpanel.run";

    private JButton mBtnSimulationRun;

    private IMenuDelegator mDelegator;

    public ControlPane() {
        mDelegator = null;
        initComponents();
        addFocusListener(this);
    }

    private void initComponents () {
        setContentPane(createContentPane());
    }

    protected Container createContentPane () {
        JPanel contentPane = new JPanel(new BorderLayout());
        contentPane.setBorder(BorderFactory.createEmptyBorder(10, 10, 10, 10));

        setPreferredSize(new Dimension(Short.MAX_VALUE, 75));
        mBtnSimulationRun = new JButton("Run");
        mBtnSimulationRun.setName(RUN);
        mBtnSimulationRun.setEnabled(false);
        mBtnSimulationRun.addActionListener(new ActionListener () {
            @Override
            public void actionPerformed(ActionEvent e) {
                if (mDelegator != null)
                    mDelegator.simulationRunPerformed(e);
            }
        });
        contentPane.add(mBtnSimulationRun, BorderLayout.CENTER);

        return contentPane;
    }

    @Override
    public void focusGained(FocusEvent e) {
        mBtnSimulationRun.requestFocus();
    }

    @Override
    public void focusLost(FocusEvent e) {
    }

    public void setDelegator(IMenuDelegator delegator) {
        mDelegator = delegator;
    }

    /* ILoadingListener */

    @Override
    public void loadingStarted() {
        mBtnSimulationRun.setEnabled(false);
    }

    @Override
    public void loadingDone() {
        mBtnSimulationRun.setEnabled(true);
    }

    /* ISimulationListener */

    @Override
    public void simulationStarted(PhspSimulator simulator) {
        mBtnSimulationRun.setEnabled(false);
    }

    @Override
    public void simulationDone() {
        mBtnSimulationRun.setEnabled(true);
    }

    @Override
    public void simulationPaused() {
    }

    @Override
    public void simulationResumed() {
    }

    /* IDesktopListener */

    @Override
    public void documentAdded(Document doc) {
        mBtnSimulationRun.setEnabled(true);
    }

    @Override
    public void documentRemoved(Document doc, boolean empty) {
        if (empty)
            mBtnSimulationRun.setEnabled(false);
    }
}
