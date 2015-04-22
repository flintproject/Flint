/* -*- Mode: Java; tab-width: 4; indent-tabs-mode: nil; c-basic-offset: 4 -*- vim:set ts=4 sw=4 sts=4 et: */

package jp.oist.flint.form;

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
import jp.oist.flint.executor.PhspSimulator;

public class ControlPane extends PeripheralPane 
    implements FocusListener, MainFrame.Listener, PhspSimulator.Listener {

    public final static String RUN = "controlpanel.run";

    public final static String TITLE  = "Control Panel";

    private final static ControlPane mInstance = new ControlPane();

    public static ControlPane getInstance () {
        return mInstance;
    }

    private JButton mBtnSimulationRun;

    private ControlPane () {
        super(TITLE);

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
                simulationRunPerformed(e);
            }
        });
        contentPane.add(mBtnSimulationRun, BorderLayout.CENTER);

        return contentPane;
    }

    protected void simulationRunPerformed (ActionEvent e) {
        FlintMenuBar menuBar = FlintMenuBar.getInstance();
        IFlintMenuBarDelegator delegator = menuBar.getDelegator();
        if (delegator != null)
            delegator.simulationRunPerformed(e);
    }

    @Override
    public void focusGained(FocusEvent e) {
        mBtnSimulationRun.requestFocus();
    }

    @Override
    public void focusLost(FocusEvent e) {
    }

    public void setSimulationRunEnabled(boolean enabled) {
        mBtnSimulationRun.setEnabled(enabled);
        FlintMenuBar.getInstance().setMenuItemEnabled(FlintMenuBar.RUN, enabled);
        FlintMenuBar.getInstance().setMenuItemEnabled(FlintMenuBar.SEND_TO_FLINT_K3, enabled);
    }

    /*
     * Implements MainFrame.Listener
     */
    @Override
    public void onModelOpened(MainFrame.Event evt) {
    }

    @Override
    public void onModelClosed(MainFrame.Event evt) {
    }

    /*
     * Implements PhspSimulator.Listener 
     */
    @Override
    public void onSimulationStarted (PhspSimulator.Event evt) {
        setSimulationRunEnabled(false);
    }

    @Override
    public void onSimulationExited (PhspSimulator.Event evt) {
        setSimulationRunEnabled(true);
    }
}
