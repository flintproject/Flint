/* -*- Mode: Java; tab-width: 4; indent-tabs-mode: nil; c-basic-offset: 4 -*- vim:set ts=4 sw=4 sts=4 et: */
package jp.oist.flint.form.job;

import jp.oist.flint.form.sub.JobWindow;
import jp.oist.flint.garuda.GarudaClient;
import jp.oist.flint.job.Job;
import jp.oist.flint.job.Progress;
import jp.oist.flint.util.DurationFormat;
import jp.oist.flint.util.PeriodFormat;
import java.awt.BorderLayout;
import java.awt.Color;
import java.awt.Dimension;
import java.awt.FlowLayout;
import java.awt.Font;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.io.IOException;
import javax.swing.BorderFactory;
import javax.swing.BoxLayout;
import javax.swing.ImageIcon;
import javax.swing.JButton;
import javax.swing.JLabel;
import javax.swing.JPanel;
import javax.swing.JProgressBar;
import javax.swing.SwingConstants;
import javax.swing.UIManager;

public class JobCell extends JPanel {

    public final static String ACTION_EXPORT = "jobcell.action.export";

    public final static String ACTION_SENDVIAGARUDA = "jobcell.action.sendviagaruda";

    public final static String ACTION_VIEW   = "jobcell.action.view";

    public final static String ACTION_CANCEL = "jobcell.action.cancel";

    private final JobWindow mJobWindow;

    private final int mIndex;

    private Number[] mCombination;

    private boolean mIsCancelled = false;

    public JobCell(JobWindow jobWindow, int index) {
        mJobWindow = jobWindow;
        mIndex = index;

        initComponents();
        setBackground(UIManager.getColor("List.background"));

        if (!GarudaClient.isRunning()) {
            btn_SendViaGaruda.setEnabled(false);
        }
    }

    @SuppressWarnings("unchecked")
    // <editor-fold defaultstate="collapsed" desc="Generated Code">//GEN-BEGIN:initComponents
    private void initComponents() {

        pnl_Top = new JPanel();
        lbl_Title = new JLabel();
        pnl_Middle = new JPanel();
        jPanel2 = new JPanel();
        mProgressBar = new JProgressBar();
        btn_Cancel = new JButton();
        pnl_Bottom = new JPanel();
        lbl_Detail = new JLabel();
        jPanel4 = new JPanel();
        btn_SendViaGaruda = new JButton();
        btn_Export = new JButton();
        btn_View = new JButton();

        setBorder(BorderFactory.createEmptyBorder(0, 10, 0, 10));
        setMinimumSize(new Dimension(0, 70));
        setPreferredSize(new Dimension(200, 70));
        setLayout(new BoxLayout(this, BoxLayout.PAGE_AXIS));

        pnl_Top.setMaximumSize(new Dimension(32767, 20));
        pnl_Top.setOpaque(false);
        pnl_Top.setPreferredSize(new Dimension(800, 20));
        pnl_Top.setLayout(new BorderLayout());
        pnl_Top.add(lbl_Title, BorderLayout.CENTER);

        add(pnl_Top);

        pnl_Middle.setMinimumSize(new Dimension(0, 0));
        pnl_Middle.setOpaque(false);
        pnl_Middle.setPreferredSize(new Dimension(800, 20));
        pnl_Middle.setLayout(new BoxLayout(pnl_Middle, BoxLayout.LINE_AXIS));

        jPanel2.setBorder(BorderFactory.createEmptyBorder(0, 0, 0, 10));
        jPanel2.setMinimumSize(new Dimension(0, 20));
        jPanel2.setOpaque(false);
        jPanel2.setPreferredSize(new Dimension(0, 23));
        jPanel2.setLayout(new BoxLayout(jPanel2, BoxLayout.LINE_AXIS));

        mProgressBar.setMinimumSize(new Dimension(0, 20));
        mProgressBar.setPreferredSize(new Dimension(0, 20));
        mProgressBar.setStringPainted(true);
        jPanel2.add(mProgressBar);

        pnl_Middle.add(jPanel2);

        btn_Cancel.setIcon(new ImageIcon(getClass().getResource("/jp/oist/flint/image/cancel.png"))); // NOI18N
        btn_Cancel.setActionCommand("jobcell.action.cancel");
        btn_Cancel.setIconTextGap(0);
        btn_Cancel.setMaximumSize(new Dimension(20, 20));
        btn_Cancel.setMinimumSize(new Dimension(20, 20));
        btn_Cancel.setPreferredSize(new Dimension(20, 20));
        btn_Cancel.addActionListener(new ActionListener() {
            public void actionPerformed(ActionEvent evt) {
                btn_CancelActionPerformed(evt);
            }
        });
        pnl_Middle.add(btn_Cancel);

        add(pnl_Middle);

        pnl_Bottom.setMaximumSize(new Dimension(65534, 30));
        pnl_Bottom.setMinimumSize(new Dimension(0, 30));
        pnl_Bottom.setOpaque(false);
        pnl_Bottom.setPreferredSize(new Dimension(800, 30));
        pnl_Bottom.setLayout(new BoxLayout(pnl_Bottom, BoxLayout.LINE_AXIS));

        lbl_Detail.setFont(new Font("Lucida Grande", 0, 12)); // NOI18N
        lbl_Detail.setForeground(Color.gray);
        lbl_Detail.setVerticalAlignment(SwingConstants.TOP);
        lbl_Detail.setMaximumSize(new Dimension(32333, 20));
        lbl_Detail.setMinimumSize(new Dimension(0, 20));
        lbl_Detail.setPreferredSize(new Dimension(600, 20));
        pnl_Bottom.add(lbl_Detail);

        jPanel4.setMaximumSize(new Dimension(250, 20));
        jPanel4.setMinimumSize(new Dimension(250, 30));
        jPanel4.setOpaque(false);
        jPanel4.setPreferredSize(new Dimension(400, 30));
        FlowLayout flowLayout1 = new FlowLayout(FlowLayout.RIGHT, 2, 0);
        flowLayout1.setAlignOnBaseline(true);
        jPanel4.setLayout(flowLayout1);

        btn_SendViaGaruda.setText("Send via Garuda");
        btn_SendViaGaruda.setActionCommand("jobcell.action.sendviagaruda");
        btn_SendViaGaruda.setMaximumSize(new Dimension(110, 20));
        btn_SendViaGaruda.setMinimumSize(new Dimension(110, 20));
        btn_SendViaGaruda.setPreferredSize(new Dimension(130, 20));
        btn_SendViaGaruda.addActionListener(new ActionListener() {
            public void actionPerformed(ActionEvent evt) {
                btn_SendViaGarudaActionPerformed(evt);
            }
        });
        jPanel4.add(btn_SendViaGaruda);

        btn_Export.setText("Export");
        btn_Export.setActionCommand("jobcell.action.export");
        btn_Export.setMaximumSize(new Dimension(75, 20));
        btn_Export.setMinimumSize(new Dimension(75, 20));
        btn_Export.setPreferredSize(new Dimension(75, 20));
        btn_Export.addActionListener(new ActionListener() {
            public void actionPerformed(ActionEvent evt) {
                btn_ExportActionPerformed(evt);
            }
        });
        jPanel4.add(btn_Export);

        btn_View.setText("View");
        btn_View.setActionCommand("jobcell.action.view");
        btn_View.setMaximumSize(new Dimension(75, 20));
        btn_View.setMinimumSize(new Dimension(75, 20));
        btn_View.setPreferredSize(new Dimension(75, 20));
        btn_View.addActionListener(new ActionListener() {
            public void actionPerformed(ActionEvent evt) {
                btn_ViewActionPerformed(evt);
            }
        });
        jPanel4.add(btn_View);

        pnl_Bottom.add(jPanel4);

        add(pnl_Bottom);
    }// </editor-fold>//GEN-END:initComponents

    private void btn_CancelActionPerformed(ActionEvent evt) {//GEN-FIRST:event_btn_CancelActionPerformed
        mJobWindow.cancelJobPerformed(mIndex);
    }//GEN-LAST:event_btn_CancelActionPerformed

    private void btn_ExportActionPerformed(ActionEvent evt) {//GEN-FIRST:event_btn_ExportActionPerformed
        mJobWindow.exportPerformed(mIndex);
    }//GEN-LAST:event_btn_ExportActionPerformed

    private void btn_SendViaGarudaActionPerformed(ActionEvent evt) {//GEN-FIRST:event_btn_SendViaGarudaActionPerformed
        mJobWindow.sendViaGarudaPerformed(mIndex);
    }//GEN-LAST:event_btn_SendViaGarudaActionPerformed

    private void btn_ViewActionPerformed(ActionEvent evt) {//GEN-FIRST:event_btn_ViewActionPerformed
        mJobWindow.plotPerformed(mIndex);
    }//GEN-LAST:event_btn_ViewActionPerformed

    public void setCombination (Number[] combination) {
        mCombination = combination;
    }

    public Number[] getCombination () {
        return mCombination;
    }

    public void setDetail (String detail) {
        lbl_Detail.setText(detail);
    }

    public void setValueIsAdjusting (boolean isAdjusting) {
        mProgressBar.getModel().setValueIsAdjusting(isAdjusting);
    }

    public boolean getValueIsAdjusting () {
        return mProgressBar.getModel().getValueIsAdjusting();
    }

    public void setProgress(Job job) {
        Progress progress = job.getProgress();
        int percent = progress.getPercent();
        mProgressBar.setValue(percent);

        StringBuilder sb = new StringBuilder();
        if (isCancelled()) {
            sb.append("cancelled | ");
        }
        sb.append(String.format("%1$3d", percent));
        sb.append(" % | ");
        sb.append(PeriodFormat.fromTo(progress.getStarted(),
                                      progress.getLastUpdated()));
        sb.append(" (");
        sb.append(DurationFormat.fromMillis(progress.getElapsedMillis()));
        sb.append(")");

        if (isFinished()) {
            btn_Cancel.setEnabled(false);
        }

        mProgressBar.setString(sb.toString());
        mProgressBar.repaint();

        if (lbl_Detail.getText().isEmpty()) {
            try {
                lbl_Detail.setText(job.getParameterDescription());
            } catch (IOException ex) {
                // give up
            }
        }
    }

    public int getProgress () {
        return mProgressBar.getValue();
    }

    public boolean isCancelled() {
        return mIsCancelled;
    }

    public void setCancelled(boolean cancelled) {
        mIsCancelled = cancelled;
        btn_Cancel.setEnabled(!cancelled);
    }

    private boolean isFinished () {
        return isCancelled() || isCompleted();
    }

    private boolean isCompleted () {
        return mProgressBar.getMaximum() == mProgressBar.getValue();
    }

    // Variables declaration - do not modify//GEN-BEGIN:variables
    private JButton btn_Cancel;
    private JButton btn_Export;
    private JButton btn_SendViaGaruda;
    private JButton btn_View;
    private JPanel jPanel2;
    private JPanel jPanel4;
    private JLabel lbl_Detail;
    private JLabel lbl_Title;
    private JProgressBar mProgressBar;
    private JPanel pnl_Bottom;
    private JPanel pnl_Middle;
    private JPanel pnl_Top;
    // End of variables declaration//GEN-END:variables
}
