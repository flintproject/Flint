/* -*- Mode: Java; tab-width: 4; indent-tabs-mode: nil; c-basic-offset: 4 -*- vim:set ts=4 sw=4 sts=4 et: */
package jp.oist.flint.form.sub;

import jp.oist.flint.control.DirectoryChooser;
import jp.oist.flint.dao.DaoException;
import jp.oist.flint.dao.SimulationDao;
import jp.oist.flint.dao.TaskDao;
import jp.oist.flint.filesystem.Filename;
import jp.oist.flint.form.IJobMenuProvider;
import jp.oist.flint.form.JobMenu;
import jp.oist.flint.form.JobPane;
import jp.oist.flint.form.MainFrame;
import jp.oist.flint.form.job.CombinationModel;
import jp.oist.flint.form.job.ExportAllWorker;
import jp.oist.flint.form.job.JobViewerComponent;
import jp.oist.flint.form.job.ParameterFilter;
import jp.oist.flint.job.Job;
import jp.oist.flint.phsp.entity.ParameterSet;
import jp.sbi.garuda.platform.commons.net.GarudaConnectionNotInitializedException;
import java.awt.CardLayout;
import java.awt.Cursor;
import java.awt.Dimension;
import java.awt.HeadlessException;
import java.awt.Window;
import java.awt.event.MouseEvent;
import java.awt.event.MouseListener;
import java.awt.event.MouseMotionListener;
import java.awt.event.WindowAdapter;
import java.awt.event.WindowEvent;
import java.beans.PropertyChangeEvent;
import java.beans.PropertyChangeListener;
import java.io.File;
import java.io.IOException;
import java.net.URL;
import java.nio.charset.StandardCharsets;
import java.sql.SQLException;
import java.util.ArrayList;
import java.util.Map;
import java.util.concurrent.CancellationException;
import java.util.concurrent.ExecutionException;
import javax.swing.DefaultListSelectionModel;
import javax.swing.ImageIcon;
import javax.swing.JOptionPane;
import javax.swing.JScrollPane;
import javax.swing.ListSelectionModel;
import javax.swing.SwingUtilities;
import javax.swing.SwingWorker;

public class JobWindow extends javax.swing.JFrame
    implements IJobMenuProvider, MouseListener, MouseMotionListener, PropertyChangeListener {

    private final static String PANELKEY_LIST  = "jobwindow.cardlayout.joblist";

    private final static String PANELKEY_VIEWER = "jobwindow.cardlayout.jobviewer";

    private final JobViewerComponent mJobViewer;

    private final SubFrame mParent;

    private final ListSelectionModel mSelectionModel;

    private final CombinationModel mDataModel;

    private final SimulationDao mSimulationDao;

    private final JobPane mJobPane;

    public JobWindow(SubFrame parent, SimulationDao simulationDao, ParameterSet parameterSet)
            throws IOException {
        super(String.format("Progress [%s]", parent.getModelCanonicalPath()));
        mParent = parent;
        mSimulationDao = simulationDao;

        URL iconUrl = getClass().getResource("/jp/oist/flint/image/icon.png");
        setIconImage(new ImageIcon(iconUrl).getImage());

        initComponents();
        mJobPane = new JobPane();
        pnl_Body.add(mJobPane, PANELKEY_LIST);
        mSelectionModel = new DefaultListSelectionModel();
        mDataModel = new CombinationModel();

        addWindowListener(new WindowAdapter() {
            @Override
            public void windowClosing(WindowEvent e) {
                setVisible(false);
            }
        }); 

        mDataModel.load(parameterSet, new ParameterFilter () {
            @Override
            public boolean accept (Number[] values) {
                return values.length > 1;
            }
        });
        mDataModel.setParameterIsDummy(parameterSet instanceof ParameterSet.Dummy);

        btn_Viewer.setVisible(!mDataModel.getParameterIsDummy());

        mJobViewer = JobViewerComponent.factory(mDataModel);
        mJobViewer.setModel(mDataModel);
        mJobViewer.setSelectionModel(mSelectionModel);
        mJobViewer.setContextMenuHandler(new JobViewerContextMenuHandler());
        mJobViewer.addMouseListener(this);
        mJobViewer.addMouseMotionListener(this);

        JScrollPane scrollPane = new JScrollPane(mJobViewer);
        scrollPane.setName(PANELKEY_VIEWER);
        scrollPane.setPreferredSize(new Dimension(640, 480));
        scrollPane.setOpaque(false);
        scrollPane.getVerticalScrollBar().setUnitIncrement(16);

        pnl_Body.add(scrollPane, PANELKEY_VIEWER);

        pack();
    }

    @SuppressWarnings("unchecked")
    // <editor-fold defaultstate="collapsed" desc="Generated Code">//GEN-BEGIN:initComponents
    private void initComponents() {

        buttonGroup1 = new javax.swing.ButtonGroup();
        pnl_Head = new javax.swing.JPanel();
        jPanel1 = new javax.swing.JPanel();
        jPanel4 = new javax.swing.JPanel();
        lbl_View = new javax.swing.JLabel();
        btn_List = new javax.swing.JToggleButton();
        btn_Viewer = new javax.swing.JToggleButton();
        jPanel2 = new javax.swing.JPanel();
        jPanel3 = new javax.swing.JPanel();
        btn_ExportAll = new javax.swing.JButton();
        pnl_Body = new javax.swing.JPanel();
        pnl_StatusBar = new javax.swing.JPanel();
        lbl_StatusBar = new javax.swing.JLabel();
        jPanel5 = new javax.swing.JPanel();
        pb_StatusBar = new javax.swing.JProgressBar();

        setDefaultCloseOperation(javax.swing.WindowConstants.DO_NOTHING_ON_CLOSE);

        pnl_Head.setBorder(javax.swing.BorderFactory.createEmptyBorder(2, 0, 2, 0));
        pnl_Head.setMaximumSize(new java.awt.Dimension(32767, 50));
        pnl_Head.setMinimumSize(new java.awt.Dimension(10, 50));
        pnl_Head.setOpaque(false);
        pnl_Head.setPreferredSize(new java.awt.Dimension(700, 50));
        pnl_Head.setRequestFocusEnabled(false);
        pnl_Head.setLayout(new java.awt.BorderLayout());

        jPanel1.setBorder(javax.swing.BorderFactory.createEmptyBorder(2, 0, 2, 0));
        jPanel1.setMaximumSize(new java.awt.Dimension(32767, 50));
        jPanel1.setMinimumSize(new java.awt.Dimension(350, 50));
        jPanel1.setOpaque(false);
        jPanel1.setPreferredSize(new java.awt.Dimension(350, 50));
        jPanel1.setRequestFocusEnabled(false);
        java.awt.FlowLayout flowLayout1 = new java.awt.FlowLayout(java.awt.FlowLayout.LEFT, 5, 0);
        flowLayout1.setAlignOnBaseline(true);
        jPanel1.setLayout(flowLayout1);

        jPanel4.setMaximumSize(new java.awt.Dimension(32767, 50));
        jPanel4.setMinimumSize(new java.awt.Dimension(300, 50));
        jPanel4.setPreferredSize(new java.awt.Dimension(300, 50));
        jPanel4.setLayout(new javax.swing.BoxLayout(jPanel4, javax.swing.BoxLayout.LINE_AXIS));

        lbl_View.setText("View : ");
        jPanel4.add(lbl_View);

        buttonGroup1.add(btn_List);
        btn_List.setSelected(true);
        btn_List.setText("List");
        btn_List.setActionCommand("jobwindow.action.joblist");
        btn_List.setAutoscrolls(true);
        btn_List.setMaximumSize(new java.awt.Dimension(100, 22));
        btn_List.setMinimumSize(new java.awt.Dimension(100, 22));
        btn_List.setPreferredSize(new java.awt.Dimension(100, 22));
        btn_List.addActionListener(new java.awt.event.ActionListener() {
            public void actionPerformed(java.awt.event.ActionEvent evt) {
                btn_ListActionPerformed(evt);
            }
        });
        jPanel4.add(btn_List);

        buttonGroup1.add(btn_Viewer);
        btn_Viewer.setText("Chart");
        btn_Viewer.setActionCommand("jobwindow.action.jobviewer");
        btn_Viewer.setAutoscrolls(true);
        btn_Viewer.setMaximumSize(new java.awt.Dimension(100, 22));
        btn_Viewer.setMinimumSize(new java.awt.Dimension(100, 22));
        btn_Viewer.setPreferredSize(new java.awt.Dimension(100, 22));
        btn_Viewer.addActionListener(new java.awt.event.ActionListener() {
            public void actionPerformed(java.awt.event.ActionEvent evt) {
                btn_ViewerActionPerformed(evt);
            }
        });
        jPanel4.add(btn_Viewer);

        jPanel1.add(jPanel4);

        pnl_Head.add(jPanel1, java.awt.BorderLayout.WEST);

        jPanel2.setBorder(javax.swing.BorderFactory.createEmptyBorder(2, 0, 2, 0));
        jPanel2.setMaximumSize(new java.awt.Dimension(32767, 50));
        jPanel2.setMinimumSize(new java.awt.Dimension(350, 50));
        jPanel2.setOpaque(false);
        jPanel2.setPreferredSize(new java.awt.Dimension(350, 50));
        jPanel2.setRequestFocusEnabled(false);
        java.awt.FlowLayout flowLayout2 = new java.awt.FlowLayout(java.awt.FlowLayout.RIGHT, 5, 0);
        flowLayout2.setAlignOnBaseline(true);
        jPanel2.setLayout(flowLayout2);

        jPanel3.setMaximumSize(new java.awt.Dimension(32767, 50));
        jPanel3.setMinimumSize(new java.awt.Dimension(110, 50));
        jPanel3.setName(""); // NOI18N
        jPanel3.setPreferredSize(new java.awt.Dimension(110, 50));
        jPanel3.setLayout(new javax.swing.BoxLayout(jPanel3, javax.swing.BoxLayout.LINE_AXIS));

        btn_ExportAll.setText("Export All");
        btn_ExportAll.setActionCommand("jobwindow.action.exportAll");
        btn_ExportAll.setEnabled(false);
        btn_ExportAll.setMaximumSize(new java.awt.Dimension(110, 22));
        btn_ExportAll.setMinimumSize(new java.awt.Dimension(110, 22));
        btn_ExportAll.setPreferredSize(new java.awt.Dimension(110, 22));
        btn_ExportAll.addActionListener(new java.awt.event.ActionListener() {
            public void actionPerformed(java.awt.event.ActionEvent evt) {
                btn_ExportAllActionPerformed(evt);
            }
        });
        jPanel3.add(btn_ExportAll);

        jPanel2.add(jPanel3);

        pnl_Head.add(jPanel2, java.awt.BorderLayout.EAST);

        getContentPane().add(pnl_Head, java.awt.BorderLayout.NORTH);

        pnl_Body.setBackground(java.awt.Color.white);
        pnl_Body.setLayout(new java.awt.CardLayout());
        getContentPane().add(pnl_Body, java.awt.BorderLayout.CENTER);

        pnl_StatusBar.setBorder(javax.swing.BorderFactory.createEtchedBorder());
        pnl_StatusBar.setPreferredSize(new java.awt.Dimension(800, 20));
        pnl_StatusBar.setLayout(new java.awt.CardLayout());

        lbl_StatusBar.setHorizontalAlignment(javax.swing.SwingConstants.LEFT);
        lbl_StatusBar.setBorder(javax.swing.BorderFactory.createEmptyBorder(0, 5, 0, 0));
        lbl_StatusBar.setHorizontalTextPosition(javax.swing.SwingConstants.LEFT);
        pnl_StatusBar.add(lbl_StatusBar, "jobwindow.statusbar.message");

        jPanel5.setLayout(new javax.swing.BoxLayout(jPanel5, javax.swing.BoxLayout.LINE_AXIS));

        pb_StatusBar.setMaximumSize(new java.awt.Dimension(32767, 18));
        pb_StatusBar.setMinimumSize(new java.awt.Dimension(20, 18));
        pb_StatusBar.setPreferredSize(new java.awt.Dimension(148, 18));
        jPanel5.add(pb_StatusBar);

        pnl_StatusBar.add(jPanel5, "jobwindow.statusbar.progress");

        getContentPane().add(pnl_StatusBar, java.awt.BorderLayout.PAGE_END);

        pack();
    }// </editor-fold>//GEN-END:initComponents

    private void btn_ViewerActionPerformed(java.awt.event.ActionEvent evt) {//GEN-FIRST:event_btn_ViewerActionPerformed
        CardLayout cardLayout = (CardLayout)pnl_Body.getLayout();
        int selectedIndex = mSelectionModel.getMinSelectionIndex();
        cardLayout.show(pnl_Body, PANELKEY_VIEWER);
        if (selectedIndex >= 0)
            mJobViewer.ensureIndexIsVisible(selectedIndex);
    }//GEN-LAST:event_btn_ViewerActionPerformed

    private void btn_ListActionPerformed(java.awt.event.ActionEvent evt) {//GEN-FIRST:event_btn_ListActionPerformed
        CardLayout cardLayout = (CardLayout)pnl_Body.getLayout();
        cardLayout.show(pnl_Body, PANELKEY_LIST);
    }//GEN-LAST:event_btn_ListActionPerformed

    private void btn_ExportAllActionPerformed(java.awt.event.ActionEvent evt) {//GEN-FIRST:event_btn_ExportAllActionPerformed
        try {
            exportAll();
        } catch (DaoException | IOException | SQLException ex) {
            showErrorDialog(ex.getMessage(),
                            "Error on exporting simulation data");
        }
    }//GEN-LAST:event_btn_ExportAllActionPerformed

    private void showMessageDialog (String msg, String title) {
        JOptionPane.showMessageDialog(this, msg, title, JOptionPane.INFORMATION_MESSAGE);
    }

    private void showErrorDialog (String msg, String title) {
        JOptionPane.showMessageDialog(this, msg, title, JOptionPane.ERROR_MESSAGE);
    }

    public void setProgress(int index, Job job) {
        mJobViewer.setProgress(index, job.getProgress());
        mJobPane.setProgress(this, index, job);

        repaint();
    }

    public void setCancelled (int index, boolean cancelled) {
        mJobPane.setCancelled(index, cancelled);
        mJobViewer.setCancelled(index, cancelled);
    }

    public int indexOf(Map<String, Number> combination) {
        Number[] target = new Number[combination.size()];
        String[] titles = mDataModel.getTitles();
        for (int i=0; i<titles.length; i++)
            target[i] = combination.get(titles[i]);

        return mDataModel.indexOf(target);
    }

    @Override
    public JobMenu getJobMenu(int index) throws DaoException, IOException, SQLException {
        TaskDao taskDao = mSimulationDao.obtainTask(mParent.getRelativeModelPath());
        int jobId = taskDao.indexOf(mDataModel.getValues(index), mDataModel.getTitles());
        return new JobMenu(this, mSimulationDao, taskDao.getTaskId(), jobId);
    }

    /*
     * Implements MouseListener, ouse
     */
    @Override
    public void mouseClicked (MouseEvent evt) {
    }

    @Override
    public void mouseEntered (MouseEvent evt) {
    }

    @Override
    public void mouseExited (MouseEvent evt) {
        lbl_StatusBar.setText("");
    }

    @Override
    public void mousePressed(MouseEvent evt) {
    }

    @Override
    public void mouseReleased(MouseEvent evt) {
    }

    /*
     *  Implements MouseMotionListener
     */
    @Override
    public void mouseMoved (MouseEvent evt) {
        if (evt.getSource() instanceof JobViewerComponent.Empty)
            return;

        Map<Integer, Number>values = mJobViewer.getValuesAtHover(evt.getPoint());
        setCursor(new Cursor(Cursor.DEFAULT_CURSOR));
        if (values == null || values.isEmpty()) {
            lbl_StatusBar.setText("");
            return;
        }

        setCursor(new Cursor(Cursor.HAND_CURSOR));
        StringBuilder sb = new StringBuilder();
        for (Map.Entry<Integer, Number> v : values.entrySet())
            sb.append(String.format("%s=%s ", 
                    mDataModel.getTitle(v.getKey()),
                    v.getValue()));


        lbl_StatusBar.setText(sb.toString());
    }

    @Override
    public void mouseDragged(MouseEvent evt) {
    }

    private void exportAll() throws DaoException, IOException, SQLException {
        InputDialogForExport inputDialog = new InputDialogForExport(this);
        String extension = inputDialog.show();
        if (extension == null)
            return;

        String defaultDir = System.getProperty("user.home");
        DirectoryChooser chooser = new DirectoryChooser(this, "Choose a target directory", defaultDir);
        if (!chooser.showDialog())
            return;

        final File selectedDir = chooser.getSelectedDirectory();

        if (!selectedDir.exists()) {
            int result = JOptionPane.showConfirmDialog(mParent,
                                                   String.format("%s does not exist; do you want to create the new directory and proceed?",
                                                                 selectedDir.getName()),
                                                   "", JOptionPane.YES_NO_OPTION);

            if (result == JOptionPane.NO_OPTION)
                return;
            if (!selectedDir.mkdirs()) {
                showErrorDialog("failed to create directory: " + selectedDir.toPath(),
                                "Error on exporting simulation data");
                return;
            }
        }

        ConfirmDialogForOverwritingFile confirmDialog = new ConfirmDialogForOverwritingFile(this);
        File listFile = new File(selectedDir, "simulation.txt");
        if (listFile.exists()) {
            int result = confirmDialog.show(listFile);
            switch (result) {
            case JOptionPane.YES_OPTION:
                if (!listFile.delete()) {
                    showErrorDialog("failed to delete " + listFile.toPath(),
                                    "Error on exporting simulation data");
                    return;
                }
                break;
            default:
                return; // quit
            }
        }

        ArrayList<File> isdFiles = new ArrayList<>();
        ArrayList<File> targetFiles = new ArrayList<>();
        ArrayList<Map<String, Number>> parameters = new ArrayList<>();
        TaskDao taskDao = mSimulationDao.obtainTask(mParent.getRelativeModelPath());
        int numJobs = taskDao.getCount();
        int numDigits = String.valueOf(numJobs).getBytes(StandardCharsets.UTF_8).length;
        for (int i=1; i<=numJobs; i++) {
            Job job = taskDao.obtainJob(i);
            File isdFile = job.getIsdFile();
            File targetFile = Filename.getVariant(mParent.getModelFile(), selectedDir, i, numDigits, extension);
            if (targetFile.exists()) {
                int result = confirmDialog.show(targetFile);
                switch (result) {
                case JOptionPane.YES_OPTION:
                    if (!targetFile.delete()) {
                        showErrorDialog("failed to delete " + targetFile.toPath(),
                                        "Error on exporting simulation data");
                        return;
                    }
                    break;
                case JOptionPane.NO_OPTION:
                    continue; // skip this file
                default:
                    return; // quit;
                }
            }
            isdFiles.add(isdFile);
            targetFiles.add(targetFile);
            parameters.add(job.getCombination());
        }

        final ExportAllWorker worker = new ExportAllWorker(mParent,
                                                           this,
                                                           extension,
                                                           listFile,
                                                           isdFiles,
                                                           targetFiles,
                                                           parameters);

        worker.addPropertyChangeListener(new PropertyChangeListener() {
            @Override
            public void propertyChange(PropertyChangeEvent evt) {
                String propertyName = evt.getPropertyName();
                Object nv = evt.getNewValue();

                if ("state".equals(propertyName)
                    && SwingWorker.StateValue.STARTED.equals(nv)) {
                    Window window = SwingUtilities.windowForComponent(mParent);
                    ((MainFrame)window).setEditable(false);
                } else if ("state".equals(propertyName)
                           && SwingWorker.StateValue.DONE.equals(nv)) {
                    Window window = SwingUtilities.windowForComponent(mParent);
                    ((MainFrame)window).setEditable(true);
                    try {
                        if (worker.get()) {
                            if (worker.isCancelled()) {
                                showMessageDialog("Exporting is cancelled.",
                                                  "Export cancelled");
                            } else {
                                showMessageDialog("Exported simulation data to " + selectedDir.getPath(),
                                                  "Export completed");
                            }
                        } else {
                            showErrorDialog("An error happened during export.",
                                            "Export failed");
                        }
                    } catch (CancellationException ex) {
                        showMessageDialog("Exporting is cancelled.",
                                          "Export cancelled");
                    } catch (InterruptedException | ExecutionException | HeadlessException ex) {
                        showErrorDialog(ex.getMessage(),
                                        "Error on exporting simulation data");
                    }
                }
            }
        });
        worker.execute();
    }

    /* PropertyChangeListener */

    @Override
    public void propertyChange(PropertyChangeEvent evt) {
        String propertyName = evt.getPropertyName();
        Object nv = evt.getNewValue();
        if ("state".equals(propertyName)) {
            if (SwingWorker.StateValue.DONE.equals(nv)) {
                btn_ExportAll.setEnabled(true);
            }
        }
    }

    /*
     * Inner classes
     */
    private class JobViewerContextMenuHandler extends JobViewerComponent.ContextMenuHandler {
        @Override
        public void handleEvent(JobViewerComponent.Event evt) {
            String action = evt.getAction();
            if (action == null)
                return;
            assert evt != null;
            assert evt.getIndex() >= 0;
            try {
                JobMenu menu = getJobMenu(evt.getIndex());
                switch (action) {
                case "view":
                    menu.view();
                    break;
                case "export":
                    menu.export();
                    break;
                case "sendViaGaruda":
                    menu.sendViaGaruda();
                    break;
                case "cancelJob":
                    menu.cancel();
                    break;
                }
            } catch (DaoException |
                     GarudaConnectionNotInitializedException |
                     IOException |
                     SQLException ex) {
                showErrorDialog(ex.getMessage(),
                                action + " failed");
            }
        }
    }

    // Variables declaration - do not modify//GEN-BEGIN:variables
    private javax.swing.JButton btn_ExportAll;
    private javax.swing.JToggleButton btn_List;
    private javax.swing.JToggleButton btn_Viewer;
    private javax.swing.ButtonGroup buttonGroup1;
    private javax.swing.JPanel jPanel1;
    private javax.swing.JPanel jPanel2;
    private javax.swing.JPanel jPanel3;
    private javax.swing.JPanel jPanel4;
    private javax.swing.JPanel jPanel5;
    private javax.swing.JLabel lbl_StatusBar;
    private javax.swing.JLabel lbl_View;
    private javax.swing.JProgressBar pb_StatusBar;
    private javax.swing.JPanel pnl_Body;
    private javax.swing.JPanel pnl_Head;
    private javax.swing.JPanel pnl_StatusBar;
    // End of variables declaration//GEN-END:variables

}
