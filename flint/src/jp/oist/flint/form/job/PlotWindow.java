/* -*- Mode: Java; tab-width: 4; indent-tabs-mode: nil; c-basic-offset: 4 -*- vim:set ts=4 sw=4 sts=4 et: */
package jp.oist.flint.form.job;

import jp.oist.flint.control.FileChooser;
import jp.oist.flint.control.VariableList;
import jp.oist.flint.dao.DaoException;
import jp.oist.flint.dao.TaskDao;
import jp.oist.flint.form.CloseByKeyStrokeAction;
import jp.oist.flint.form.sub.IChartController;
import jp.oist.flint.form.sub.IChartSetting;
import jp.oist.flint.job.Job;
import jp.oist.flint.job.Progress;
import jp.oist.flint.plot.gnuplot.GnuPlotter;
import jp.oist.flint.plotter.IPlotter;
import jp.oist.flint.plotter.PlotterLoadException;
import jp.oist.flint.plotter.PlotterLoader;
import jp.oist.flint.theme.Icon;
import jp.oist.flint.util.LegendCollection;
import jp.oist.flint.util.ListItemModel;
import jp.oist.flint.util.ListItemTransferHandler;
import jp.oist.flint.util.Utility;
import jp.oist.flint.view.ChartListener;
import jp.oist.flint.view.ChartUpdater;
import jp.oist.flint.view.Summary;
import jp.oist.flint.view.SummaryAdapter;
import jp.physiome.Ipc;
import org.jfree.chart.ChartPanel;
import java.awt.BorderLayout;
import java.awt.HeadlessException;
import java.beans.PropertyChangeEvent;
import java.beans.PropertyChangeListener;
import java.io.File;
import java.io.IOException;
import java.math.BigDecimal;
import java.sql.SQLException;
import java.util.HashMap;
import java.util.LinkedHashMap;
import java.util.List;
import java.util.Locale;
import java.util.concurrent.ExecutionException;
import javax.swing.JDialog;
import javax.swing.JOptionPane;
import javax.swing.SwingWorker;

public class PlotWindow extends javax.swing.JFrame 
    implements IChartController, IChartSetting {

    private final static String LABEL_SEPARATOR = "@";

    private HashMap<String, String> mLegendMap;
    private final HashMap<String, Integer> mMap;

    private VariableList mVariables;
    private VariableList mXVariables;
    private VariableList mYVariables;
    private VariableList mY2Variables;

    private final Job mJob;

    private final File mModelFile;
    private final File mIsdFile;
    private final File mCsvFile;

    public PlotWindow(File modelFile, String title, TaskDao taskDao, int jobId)
        throws DaoException, IOException, SQLException {
        super(title);

        setIconImage(Icon.getImage());

        mJob = taskDao.obtainJob(jobId);
        mModelFile = modelFile;
        mIsdFile = mJob.getIsdFile();
        mCsvFile = new File(mIsdFile.getParent(), "csv");

        mLegendMap = new HashMap<>();
        mMap = new HashMap<>();

        initComponents();

        jSplitPane1.setDividerLocation(0.20);

        CloseByKeyStrokeAction.register(this);
    }

    public void setExportCsvAndPlotEnabled(boolean enabled) {
        btn_Plot.setEnabled(enabled);
    }

    public boolean isXLogSelected () {
        return  checkBoxXlog.isSelected();
    }

    public boolean isY1LogSelected () {
        return checkBoxY1log.isSelected();
    }

    public boolean isY2LogSelected () {
        return checkBoxY2log.isSelected();
    }

    public boolean isLegendSelected () {
        return checkBoxLegend.isSelected();
    }

    public void setVariableList (VariableList vl) {
        js_VariableList.getViewport().setView(vl);
    }

    public void setXVariableList (VariableList vl) {
        js_VariableXList.getViewport().setView(vl);
    }

    public void setY1VariableList (VariableList vl) {
        js_VariableYList.getViewport().setView(vl);
    }

    public void setY2VariableList (VariableList vl) {
        js_VariableY2List.getViewport().setView(vl);
    }

    public void setTrackInfo (String txt) {
        lbl_trackInfo.setText(txt);
    }

    public boolean outputWindowIsSelected () {
        return radio_OutputWindow.isSelected();
    }

    @Override
    public String getTerminalType () {
        if (outputWindowIsSelected()) {
            return GnuPlotter.TerminalType.WINDOW.name();
        } else {
            return cmb_TerminalType.getSelectedItem().toString();
        }
    }

    @Override
    public String getOutputFilePath() {
        return txt_OutputFilePath.getText().replace("\\", "/");
    }

    @Override
    public boolean isXRangeDefault () {
        return btn_Range_X_Default.isSelected();
    }

    @Override
    public boolean isYRangeDefault () {
        return btn_Range_Y_Default.isSelected();
    }

    @Override
    public boolean isY2RangeDefault () {
        return btn_Range_Y2_Default.isSelected();
    }

    @Override
    public double getXStart () {
        return Double.parseDouble(spn_Range_X_Start.getValue().toString());
    }

    @Override
    public double getXEnd () {
        return Double.parseDouble(spn_Range_X_End.getValue().toString());
    }

    @Override
    public double getYStart () {
        return Double.parseDouble(spn_Range_Y_Start.getValue().toString());
    }

    @Override
    public double getYEnd() {
        return Double.parseDouble(spn_Range_Y_End.getValue().toString());
    }

    @Override
    public double getY2Start () {
        return Double.parseDouble(spn_Range_Y2_Start.getValue().toString());
    }

    @Override
    public double getY2End () {
        return Double.parseDouble(spn_Range_Y2_End.getValue().toString());
    }

    @Override
    public boolean isXLegendDefault () {
        return btn_Legend_X_Default.isSelected();
    }

    @Override
    public boolean isYLegendDefault () {
        return btn_Legend_Y_Default.isSelected();
    }

    @Override
    public boolean isY2LegendDefault () {
        return btn_Legend_Y2_Default.isSelected();
    }

    @Override
    public String getXLegend () {
        return txt_Legend_X.getText();
    }

    @Override
    public String getYLegend () {
        return txt_Legend_Y.getText();
    }

    @Override
    public String getY2Legend () {
        return txt_Legend_Y2.getText();
    }

    @Override
    public boolean showsKey () {
        return chk_Disp_Introductory.isSelected();
    }

    @Override
    public String getLegend(String key) {
        return mLegendMap.get(key);
    }

    @Override
    public int getTrackIndex(Object key) {
        return mMap.get(key.toString());
    }

    @Override
    public ListItemModel getXListItemModel() {
        return (ListItemModel)mXVariables.getModel();
    }

    @Override
    public ListItemModel getY2ListItemModel() {
        return (ListItemModel)mY2Variables.getModel();
    }

    @Override
    public ListItemModel getYListItemModel() {
        return (ListItemModel)mYVariables.getModel();
    }

    @Override
    public boolean isLegendChecked() {
        return checkBoxLegend.isSelected();
    }

    @Override
    public boolean isXlogChecked() {
        return checkBoxXlog.isSelected();
    }

    @Override
    public boolean isY1logChecked() {
        return checkBoxY1log.isSelected();
    }

    @Override
    public boolean isY2logChecked() {
        return checkBoxY2log.isSelected();
    }

    @Override
    public IChartSetting getChartSetting () {
        return this;
    }

    @Override
    public File getModelFile () {
        return mModelFile;
    }

    @Override
    public File getIsdFile() {
        return mIsdFile;
    }

    @Override
    public File getCsvFile () {
        return mCsvFile;
    }

    @Override
    public void renderPlot() {
        clearChart(); 
        ChartUpdater updater = new ChartUpdater(this);
        updater.execute();
    }

    @Override
    public void clearChart () {
        pnl_PlotArea.removeAll();            
        pnl_Track.validate();
        pnl_Track.repaint();
    }

    @Override
    public void displayChart(ChartPanel panel) {
        pnl_PlotArea.removeAll();
        pnl_PlotArea.add(panel, BorderLayout.CENTER);
        pnl_Track.validate();
        pnl_Track.repaint();
    }

    @Override
    public void displaySummary(Summary summary) {
        Progress progress = mJob.getProgress();
        lbl_trackInfo.setText(summary.getName() + " "
                              + " max:" + Utility.getEfficientRound(new BigDecimal(summary.getMax()), 3)
                              + " min:" + Utility.getEfficientRound(new BigDecimal(summary.getMin()), 3)
                              + " [" + progress.getPercent() + " %]");
    }

    public boolean processSimulationTrack(final Ipc.SimulationTrack simTrack) {
        int numTracks = simTrack.getKeyCount();
        if (numTracks == 0) return false;

        List<String> xKeys = null;
        List<String> yKeys = null;
        List<String> y2Keys = null;
        if (!mMap.isEmpty()) { // if rerunning simulation
            ListItemModel xModel = (ListItemModel)mXVariables.getModel();
            if (xModel != null) xKeys = xModel.keys();
            ListItemModel yModel = (ListItemModel)mYVariables.getModel();
            if (yModel != null) yKeys = yModel.keys();
            ListItemModel y2Model = (ListItemModel)mY2Variables.getModel();
            if (y2Model != null) y2Keys = y2Model.keys();
        }

        mMap.clear();

        LinkedHashMap<String, String> xTrack = new LinkedHashMap<>();
        LinkedHashMap<String, String> yTrack = new LinkedHashMap<>();
        LinkedHashMap<String, String> y2Track = new LinkedHashMap<>();
        LinkedHashMap<String, String> variableTrack = new LinkedHashMap<>();

        LegendCollection lc = new LegendCollection();
        int index = 0;
        for (int i = 0; i < numTracks; i++) {
            int col = simTrack.getCol(i);
            int row = simTrack.getRow(i);
            String barekey = simTrack.getKey(i);
            String barename = simTrack.getName(i);
            String label = simTrack.getLabel(i);
            for (int k=0;k<col;k++) { // column-major
                for (int j=0;j<row;j++) {
                    String suffix = (col == 1) ? ((row == 1) ? "" : "[" + (j+1) + "]") : ((row == 1) ? ("[" + (k+1) + "]") : ("[" + (k+1) + ", " + (j+1) + "]"));
                    String key = barekey + suffix;
                    String name = barename + suffix;
                    String title = (label.length() == 0) ? name : name + LABEL_SEPARATOR + label;

                    lc.register(key, title, index);
                    mMap.put(key, index);
                    index += 1;

                    if ( (xKeys == null && "time".equals(name)) || (xKeys != null && xKeys.contains(key)) ) {
                        xTrack.put(key, title);
                    } else if ( yKeys != null && yKeys.contains(key) ) {
                        yTrack.put(key, title);
                    } else if ( y2Keys != null && y2Keys.contains(key) ) {
                        y2Track.put(key, title);
                    } else {
                        variableTrack.put(key, title);
                    }
                }
            }
        }
        mLegendMap = lc.toHashMap(); // each legend has to be unique in XYSeriesCollection

        ListItemTransferHandler h = new ListItemTransferHandler();

        mVariables = Utility.makeVariableList("mVariables", variableTrack, h);
        mVariables.addMouseListener(new SummaryAdapter(this, mVariables));
        mVariables.getModel().addListDataListener(new ChartListener(this));
        js_VariableList.getViewport().setView(mVariables);

        mYVariables = Utility.makeVariableList("mYVariables", yTrack, h);
        mYVariables.addMouseListener(new SummaryAdapter(this, mYVariables));
        mYVariables.getModel().addListDataListener(new ChartListener(this));
        js_VariableYList.getViewport().setView(mYVariables);

        mY2Variables = Utility.makeVariableList("mY2Variables", y2Track, h);
        mY2Variables.addMouseListener(new SummaryAdapter(this, mY2Variables));
        mY2Variables.getModel().addListDataListener(new ChartListener(this));
        js_VariableY2List.getViewport().setView(mY2Variables);

        mXVariables = Utility.makeVariableList("mXVariables", xTrack, h, false);
        mXVariables.addMouseListener(new SummaryAdapter(this, mXVariables));
        mXVariables.getModel().addListDataListener(new ChartListener(this));
        js_VariableXList.getViewport().setView(mXVariables);

//        Utility.setEnabledJComponent(this, true);
        setExportCsvAndPlotEnabled(true);
        chgOutPutFilePanel();
        chgRangeX();
        chgRangeY();
        chgRangeY2();
        chgLegendX();
        chgLegendY();
        chgLegendY2();
        return true;
    }

    public void chgOutPutFilePanel() {
        if (radio_OutputWindow.isSelected()) {
            Utility.setEnabledJComponent(pnl_OutputFile, false);
        } else {
            Utility.setEnabledJComponent(pnl_OutputFile, true);
        }
    }

    public void chgRangeX() {
        if (btn_Range_X_Default.isSelected()) {
            spn_Range_X_Start.setEnabled(false);
            spn_Range_X_End.setEnabled(false);
        } else {
            spn_Range_X_Start.setEnabled(true);
            spn_Range_X_End.setEnabled(true);
        }
    }

    public void chgRangeY() {
        if (btn_Range_Y_Default.isSelected()) {
            spn_Range_Y_Start.setEnabled(false);
            spn_Range_Y_End.setEnabled(false);
        } else {
            spn_Range_Y_Start.setEnabled(true);
            spn_Range_Y_End.setEnabled(true);
        }
    }

    public void chgRangeY2() {
        if (btn_Range_Y2_Default.isSelected()) {
            spn_Range_Y2_Start.setEnabled(false);
            spn_Range_Y2_End.setEnabled(false);
        } else {
            spn_Range_Y2_Start.setEnabled(true);
            spn_Range_Y2_End.setEnabled(true);
        }
    }

    public void chgLegendX () {
        if (btn_Legend_X_Default.isSelected()) {
            txt_Legend_X.setEnabled(false);
        } else {
            txt_Legend_X.setEnabled(true);
        }
    }

    public void chgLegendY () {
        if (btn_Legend_Y_Default.isSelected()) {
            txt_Legend_Y.setEnabled(false);
        } else {
            txt_Legend_Y.setEnabled(true);
        }
    }

    public void chgLegendY2 () {
        if (btn_Legend_Y2_Default.isSelected()) {
            txt_Legend_Y2.setEnabled(false);
        } else {
            txt_Legend_Y2.setEnabled(true);
        }
    }

    public void setPlotterSettingTabEnabled (boolean enabledFlg) {
        pnl_Content.setEnabledAt(1, enabledFlg);
        pnl_Content.setEnabledAt(2, enabledFlg);
        pnl_Content.setEnabledAt(3, enabledFlg);
    }
    
    // <editor-fold defaultstate="collapsed" desc="Generated Code">//GEN-BEGIN:initComponents
    private void initComponents() {

        btngrp_Target = new javax.swing.ButtonGroup();
        btngrp_RangeX = new javax.swing.ButtonGroup();
        btngrp_RangeY1 = new javax.swing.ButtonGroup();
        btngrp_RangeY2 = new javax.swing.ButtonGroup();
        btngrp_LegendX = new javax.swing.ButtonGroup();
        btngrp_LegendY1 = new javax.swing.ButtonGroup();
        btngrp_LegendY2 = new javax.swing.ButtonGroup();
        pnl_Wrapper = new javax.swing.JPanel();
        pnl_Content = new javax.swing.JTabbedPane();
        pnl_Track = new javax.swing.JPanel();
        jSplitPane1 = new javax.swing.JSplitPane();
        jPanel1 = new javax.swing.JPanel();
        js_VariableList = new javax.swing.JScrollPane();
        lbl_Variables = new javax.swing.JLabel();
        jPanel2 = new javax.swing.JPanel();
        js_VariableYList = new javax.swing.JScrollPane();
        pnl_PlotArea = new javax.swing.JPanel();
        js_VariableY2List = new javax.swing.JScrollPane();
        js_VariableXList = new javax.swing.JScrollPane();
        lbl_Abscissa = new javax.swing.JLabel();
        checkBoxXlog = new javax.swing.JCheckBox();
        checkBoxLegend = new javax.swing.JCheckBox();
        lbl_Y1 = new javax.swing.JLabel();
        checkBoxY1log = new javax.swing.JCheckBox();
        checkBoxY2log = new javax.swing.JCheckBox();
        lbl_Y2 = new javax.swing.JLabel();
        pnl_Target = new javax.swing.JPanel();
        pnl_OutputFile = new javax.swing.JPanel();
        lbl_Format = new javax.swing.JLabel();
        lbl_Path = new javax.swing.JLabel();
        cmb_TerminalType = new javax.swing.JComboBox();
        txt_OutputFilePath = new javax.swing.JTextField();
        btn_OutputFilePath = new javax.swing.JButton();
        radio_OutputWindow = new javax.swing.JRadioButton();
        radio_OutputFile = new javax.swing.JRadioButton();
        pnl_Range = new javax.swing.JPanel();
        pnl_RangeForm = new javax.swing.JPanel();
        pnl_RangeRow1 = new javax.swing.JPanel();
        btn_Range_X_Default = new javax.swing.JRadioButton();
        btn_Range_X_Specified = new javax.swing.JRadioButton();
        spn_Range_X_Start = new javax.swing.JSpinner();
        spn_Range_X_End = new javax.swing.JSpinner();
        pnl_RangeRow2 = new javax.swing.JPanel();
        btn_Range_Y_Default = new javax.swing.JRadioButton();
        spn_Range_Y_Start = new javax.swing.JSpinner();
        btn_Range_Y_Specified = new javax.swing.JRadioButton();
        spn_Range_Y_End = new javax.swing.JSpinner();
        pnl_RangeRow3 = new javax.swing.JPanel();
        btn_Range_Y2_Specified = new javax.swing.JRadioButton();
        spn_Range_Y2_End = new javax.swing.JSpinner();
        btn_Range_Y2_Default = new javax.swing.JRadioButton();
        spn_Range_Y2_Start = new javax.swing.JSpinner();
        pnl_RangeMarginBottom = new javax.swing.JPanel();
        pnl_Legend = new javax.swing.JPanel();
        pnl_LegendTop = new javax.swing.JPanel();
        chk_Disp_Introductory = new javax.swing.JCheckBox();
        pnl_LegendCenter = new javax.swing.JPanel();
        pnl_LegendRow1 = new javax.swing.JPanel();
        btn_Legend_X_Default = new javax.swing.JRadioButton();
        btn_Legend_X_Specified = new javax.swing.JRadioButton();
        txt_Legend_X = new javax.swing.JTextField();
        pnl_LegendRow2 = new javax.swing.JPanel();
        btn_Legend_Y_Specified = new javax.swing.JRadioButton();
        btn_Legend_Y_Default = new javax.swing.JRadioButton();
        txt_Legend_Y = new javax.swing.JTextField();
        pnl_LegendRow3 = new javax.swing.JPanel();
        btn_Legend_Y2_Default = new javax.swing.JRadioButton();
        btn_Legend_Y2_Specified = new javax.swing.JRadioButton();
        txt_Legend_Y2 = new javax.swing.JTextField();
        pnl_LegendMarginBottom = new javax.swing.JPanel();
        pnl_Foot = new javax.swing.JPanel();
        btn_Plot = new javax.swing.JButton();
        panelStatusBar = new javax.swing.JPanel();
        lbl_trackInfo = new javax.swing.JLabel();

        setDefaultCloseOperation(javax.swing.WindowConstants.DISPOSE_ON_CLOSE);
        setMinimumSize(new java.awt.Dimension(700, 530));
        setPreferredSize(new java.awt.Dimension(700, 530));

        pnl_Wrapper.setMinimumSize(new java.awt.Dimension(700, 530));
        pnl_Wrapper.setPreferredSize(new java.awt.Dimension(700, 500));
        pnl_Wrapper.setLayout(new java.awt.BorderLayout());

        pnl_Content.setMinimumSize(new java.awt.Dimension(680, 460));
        pnl_Content.setPreferredSize(new java.awt.Dimension(680, 460));

        lbl_Variables.setFont(new java.awt.Font("Osaka", 0, 13)); // NOI18N
        lbl_Variables.setText("Variables");

        javax.swing.GroupLayout jPanel1Layout = new javax.swing.GroupLayout(jPanel1);
        jPanel1.setLayout(jPanel1Layout);
        jPanel1Layout.setHorizontalGroup(
            jPanel1Layout.createParallelGroup(javax.swing.GroupLayout.Alignment.LEADING)
            .addComponent(lbl_Variables, javax.swing.GroupLayout.Alignment.TRAILING, javax.swing.GroupLayout.DEFAULT_SIZE, javax.swing.GroupLayout.DEFAULT_SIZE, Short.MAX_VALUE)
            .addGroup(jPanel1Layout.createParallelGroup(javax.swing.GroupLayout.Alignment.LEADING)
                .addComponent(js_VariableList, javax.swing.GroupLayout.DEFAULT_SIZE, 63, Short.MAX_VALUE))
        );
        jPanel1Layout.setVerticalGroup(
            jPanel1Layout.createParallelGroup(javax.swing.GroupLayout.Alignment.LEADING)
            .addGroup(jPanel1Layout.createSequentialGroup()
                .addComponent(lbl_Variables)
                .addGap(0, 373, Short.MAX_VALUE))
            .addGroup(jPanel1Layout.createParallelGroup(javax.swing.GroupLayout.Alignment.LEADING)
                .addGroup(jPanel1Layout.createSequentialGroup()
                    .addGap(22, 22, 22)
                    .addComponent(js_VariableList, javax.swing.GroupLayout.DEFAULT_SIZE, 356, Short.MAX_VALUE)
                    .addContainerGap()))
        );

        jSplitPane1.setLeftComponent(jPanel1);

        pnl_PlotArea.setBackground(new java.awt.Color(255, 255, 255));
        pnl_PlotArea.setLayout(new java.awt.BorderLayout());

        lbl_Abscissa.setFont(new java.awt.Font("Osaka", 0, 13)); // NOI18N
        lbl_Abscissa.setText("Abscissa");

        checkBoxXlog.setFont(new java.awt.Font("Osaka", 0, 13)); // NOI18N
        checkBoxXlog.setText("log");
        checkBoxXlog.addActionListener(new java.awt.event.ActionListener() {
            @Override
            public void actionPerformed(java.awt.event.ActionEvent evt) {
                checkBoxXlogActionPerformed(evt);
            }
        });

        checkBoxLegend.setFont(new java.awt.Font("Osaka", 0, 13)); // NOI18N
        checkBoxLegend.setSelected(true);
        checkBoxLegend.setText("Legend");
        checkBoxLegend.addActionListener(new java.awt.event.ActionListener() {
            @Override
            public void actionPerformed(java.awt.event.ActionEvent evt) {
                checkBoxLegendActionPerformed(evt);
            }
        });

        lbl_Y1.setFont(new java.awt.Font("Osaka", 0, 13)); // NOI18N
        lbl_Y1.setText("Y1");

        checkBoxY1log.setFont(new java.awt.Font("Osaka", 0, 13)); // NOI18N
        checkBoxY1log.setText("log");
        checkBoxY1log.addActionListener(new java.awt.event.ActionListener() {
            @Override
            public void actionPerformed(java.awt.event.ActionEvent evt) {
                checkBoxY1logActionPerformed(evt);
            }
        });

        checkBoxY2log.setFont(new java.awt.Font("Osaka", 0, 13)); // NOI18N
        checkBoxY2log.setText("log");
        checkBoxY2log.addActionListener(new java.awt.event.ActionListener() {
            @Override
            public void actionPerformed(java.awt.event.ActionEvent evt) {
                checkBoxY2logActionPerformed(evt);
            }
        });

        lbl_Y2.setFont(new java.awt.Font("Osaka", 0, 13)); // NOI18N
        lbl_Y2.setText("Y2");

        javax.swing.GroupLayout jPanel2Layout = new javax.swing.GroupLayout(jPanel2);
        jPanel2.setLayout(jPanel2Layout);
        jPanel2Layout.setHorizontalGroup(
            jPanel2Layout.createParallelGroup(javax.swing.GroupLayout.Alignment.LEADING)
            .addGroup(jPanel2Layout.createSequentialGroup()
                .addContainerGap()
                .addGroup(jPanel2Layout.createParallelGroup(javax.swing.GroupLayout.Alignment.LEADING)
                    .addComponent(js_VariableYList, javax.swing.GroupLayout.PREFERRED_SIZE, 100, javax.swing.GroupLayout.PREFERRED_SIZE)
                    .addGroup(jPanel2Layout.createSequentialGroup()
                        .addComponent(lbl_Y1)
                        .addPreferredGap(javax.swing.LayoutStyle.ComponentPlacement.RELATED)
                        .addComponent(checkBoxY1log)))
                .addPreferredGap(javax.swing.LayoutStyle.ComponentPlacement.RELATED)
                .addGroup(jPanel2Layout.createParallelGroup(javax.swing.GroupLayout.Alignment.LEADING)
                    .addGroup(jPanel2Layout.createSequentialGroup()
                        .addComponent(lbl_Abscissa)
                        .addPreferredGap(javax.swing.LayoutStyle.ComponentPlacement.RELATED)
                        .addComponent(checkBoxXlog)
                        .addPreferredGap(javax.swing.LayoutStyle.ComponentPlacement.RELATED, javax.swing.GroupLayout.DEFAULT_SIZE, Short.MAX_VALUE)
                        .addComponent(checkBoxLegend))
                    .addComponent(pnl_PlotArea, javax.swing.GroupLayout.Alignment.TRAILING, javax.swing.GroupLayout.DEFAULT_SIZE, javax.swing.GroupLayout.DEFAULT_SIZE, Short.MAX_VALUE)
                    .addComponent(js_VariableXList, javax.swing.GroupLayout.Alignment.TRAILING, javax.swing.GroupLayout.DEFAULT_SIZE, 351, Short.MAX_VALUE))
                .addPreferredGap(javax.swing.LayoutStyle.ComponentPlacement.RELATED)
                .addGroup(jPanel2Layout.createParallelGroup(javax.swing.GroupLayout.Alignment.LEADING)
                    .addComponent(js_VariableY2List, javax.swing.GroupLayout.PREFERRED_SIZE, 100, javax.swing.GroupLayout.PREFERRED_SIZE)
                    .addGroup(jPanel2Layout.createSequentialGroup()
                        .addComponent(lbl_Y2)
                        .addPreferredGap(javax.swing.LayoutStyle.ComponentPlacement.RELATED)
                        .addComponent(checkBoxY2log)))
                .addContainerGap())
        );
        jPanel2Layout.setVerticalGroup(
            jPanel2Layout.createParallelGroup(javax.swing.GroupLayout.Alignment.LEADING)
            .addGroup(javax.swing.GroupLayout.Alignment.TRAILING, jPanel2Layout.createSequentialGroup()
                .addGroup(jPanel2Layout.createParallelGroup(javax.swing.GroupLayout.Alignment.BASELINE)
                    .addComponent(lbl_Y1)
                    .addComponent(checkBoxY1log)
                    .addComponent(lbl_Y2)
                    .addComponent(checkBoxY2log))
                .addGroup(jPanel2Layout.createParallelGroup(javax.swing.GroupLayout.Alignment.LEADING)
                    .addComponent(pnl_PlotArea, javax.swing.GroupLayout.DEFAULT_SIZE, javax.swing.GroupLayout.DEFAULT_SIZE, Short.MAX_VALUE)
                    .addComponent(js_VariableY2List, javax.swing.GroupLayout.DEFAULT_SIZE, 279, Short.MAX_VALUE)
                    .addComponent(js_VariableYList))
                .addPreferredGap(javax.swing.LayoutStyle.ComponentPlacement.UNRELATED)
                .addGroup(jPanel2Layout.createParallelGroup(javax.swing.GroupLayout.Alignment.BASELINE)
                    .addComponent(lbl_Abscissa)
                    .addComponent(checkBoxXlog)
                    .addComponent(checkBoxLegend))
                .addPreferredGap(javax.swing.LayoutStyle.ComponentPlacement.RELATED)
                .addComponent(js_VariableXList, javax.swing.GroupLayout.PREFERRED_SIZE, 39, javax.swing.GroupLayout.PREFERRED_SIZE)
                .addGap(10, 10, 10))
        );

        jSplitPane1.setRightComponent(jPanel2);

        javax.swing.GroupLayout pnl_TrackLayout = new javax.swing.GroupLayout(pnl_Track);
        pnl_Track.setLayout(pnl_TrackLayout);
        pnl_TrackLayout.setHorizontalGroup(
            pnl_TrackLayout.createParallelGroup(javax.swing.GroupLayout.Alignment.LEADING)
            .addGroup(javax.swing.GroupLayout.Alignment.TRAILING, pnl_TrackLayout.createSequentialGroup()
                .addContainerGap()
                .addComponent(jSplitPane1)
                .addContainerGap())
        );
        pnl_TrackLayout.setVerticalGroup(
            pnl_TrackLayout.createParallelGroup(javax.swing.GroupLayout.Alignment.LEADING)
            .addGroup(pnl_TrackLayout.createSequentialGroup()
                .addContainerGap()
                .addComponent(jSplitPane1, javax.swing.GroupLayout.DEFAULT_SIZE, 390, Short.MAX_VALUE)
                .addContainerGap())
        );

        pnl_Content.addTab("Track", pnl_Track);

        pnl_Target.setPreferredSize(new java.awt.Dimension(633, 244));

        pnl_OutputFile.setBorder(javax.swing.BorderFactory.createLineBorder(new java.awt.Color(0, 0, 0)));
        pnl_OutputFile.setPreferredSize(new java.awt.Dimension(597, 129));

        lbl_Format.setText("Format: ");

        lbl_Path.setText("Path:");

        cmb_TerminalType.setModel(new javax.swing.DefaultComboBoxModel(new String[] { "PDF", "PNG", "EPS" }));

        txt_OutputFilePath.setFont(new java.awt.Font("Osaka", 0, 13)); // NOI18N

        btn_OutputFilePath.setText("Select");
        btn_OutputFilePath.addActionListener(new java.awt.event.ActionListener() {
            public void actionPerformed(java.awt.event.ActionEvent evt) {
                btn_OutputFilePathActionPerformed(evt);
            }
        });

        javax.swing.GroupLayout pnl_OutputFileLayout = new javax.swing.GroupLayout(pnl_OutputFile);
        pnl_OutputFile.setLayout(pnl_OutputFileLayout);
        pnl_OutputFileLayout.setHorizontalGroup(
            pnl_OutputFileLayout.createParallelGroup(javax.swing.GroupLayout.Alignment.LEADING)
            .addGroup(pnl_OutputFileLayout.createSequentialGroup()
                .addContainerGap()
                .addGroup(pnl_OutputFileLayout.createParallelGroup(javax.swing.GroupLayout.Alignment.LEADING)
                    .addComponent(lbl_Format)
                    .addComponent(lbl_Path))
                .addPreferredGap(javax.swing.LayoutStyle.ComponentPlacement.UNRELATED)
                .addGroup(pnl_OutputFileLayout.createParallelGroup(javax.swing.GroupLayout.Alignment.LEADING)
                    .addComponent(cmb_TerminalType, javax.swing.GroupLayout.PREFERRED_SIZE, 99, javax.swing.GroupLayout.PREFERRED_SIZE)
                    .addGroup(pnl_OutputFileLayout.createSequentialGroup()
                        .addComponent(txt_OutputFilePath, javax.swing.GroupLayout.PREFERRED_SIZE, 347, javax.swing.GroupLayout.PREFERRED_SIZE)
                        .addPreferredGap(javax.swing.LayoutStyle.ComponentPlacement.RELATED)
                        .addComponent(btn_OutputFilePath, javax.swing.GroupLayout.PREFERRED_SIZE, 95, javax.swing.GroupLayout.PREFERRED_SIZE)))
                .addContainerGap(javax.swing.GroupLayout.DEFAULT_SIZE, Short.MAX_VALUE))
        );
        pnl_OutputFileLayout.setVerticalGroup(
            pnl_OutputFileLayout.createParallelGroup(javax.swing.GroupLayout.Alignment.LEADING)
            .addGroup(pnl_OutputFileLayout.createSequentialGroup()
                .addGap(20, 20, 20)
                .addGroup(pnl_OutputFileLayout.createParallelGroup(javax.swing.GroupLayout.Alignment.BASELINE)
                    .addComponent(lbl_Format)
                    .addComponent(cmb_TerminalType, javax.swing.GroupLayout.PREFERRED_SIZE, javax.swing.GroupLayout.DEFAULT_SIZE, javax.swing.GroupLayout.PREFERRED_SIZE))
                .addPreferredGap(javax.swing.LayoutStyle.ComponentPlacement.RELATED, 19, Short.MAX_VALUE)
                .addGroup(pnl_OutputFileLayout.createParallelGroup(javax.swing.GroupLayout.Alignment.BASELINE)
                    .addComponent(lbl_Path)
                    .addComponent(txt_OutputFilePath, javax.swing.GroupLayout.PREFERRED_SIZE, javax.swing.GroupLayout.DEFAULT_SIZE, javax.swing.GroupLayout.PREFERRED_SIZE)
                    .addComponent(btn_OutputFilePath))
                .addGap(26, 26, 26))
        );

        btngrp_Target.add(radio_OutputWindow);
        radio_OutputWindow.setFont(new java.awt.Font("Osaka", 0, 13)); // NOI18N
        radio_OutputWindow.setSelected(true);
        radio_OutputWindow.setText("Window");
        radio_OutputWindow.addActionListener(new java.awt.event.ActionListener() {
            public void actionPerformed(java.awt.event.ActionEvent evt) {
                radio_OutputWindowActionPerformed(evt);
            }
        });

        btngrp_Target.add(radio_OutputFile);
        radio_OutputFile.setFont(new java.awt.Font("Osaka", 0, 13)); // NOI18N
        radio_OutputFile.setText("File");
        radio_OutputFile.addActionListener(new java.awt.event.ActionListener() {
            @Override
            public void actionPerformed(java.awt.event.ActionEvent evt) {
                radio_OutputFileActionPerformed(evt);
            }
        });

        javax.swing.GroupLayout pnl_TargetLayout = new javax.swing.GroupLayout(pnl_Target);
        pnl_Target.setLayout(pnl_TargetLayout);
        pnl_TargetLayout.setHorizontalGroup(
            pnl_TargetLayout.createParallelGroup(javax.swing.GroupLayout.Alignment.LEADING)
            .addGroup(pnl_TargetLayout.createSequentialGroup()
                .addGap(38, 38, 38)
                .addComponent(pnl_OutputFile, javax.swing.GroupLayout.DEFAULT_SIZE, 605, Short.MAX_VALUE)
                .addGap(49, 49, 49))
            .addGroup(pnl_TargetLayout.createSequentialGroup()
                .addGap(30, 30, 30)
                .addGroup(pnl_TargetLayout.createParallelGroup(javax.swing.GroupLayout.Alignment.LEADING)
                    .addComponent(radio_OutputFile)
                    .addComponent(radio_OutputWindow))
                .addContainerGap(596, Short.MAX_VALUE))
        );
        pnl_TargetLayout.setVerticalGroup(
            pnl_TargetLayout.createParallelGroup(javax.swing.GroupLayout.Alignment.LEADING)
            .addGroup(javax.swing.GroupLayout.Alignment.TRAILING, pnl_TargetLayout.createSequentialGroup()
                .addGap(43, 43, 43)
                .addComponent(radio_OutputWindow)
                .addGap(18, 18, 18)
                .addComponent(radio_OutputFile)
                .addGap(18, 18, 18)
                .addComponent(pnl_OutputFile, javax.swing.GroupLayout.PREFERRED_SIZE, javax.swing.GroupLayout.DEFAULT_SIZE, javax.swing.GroupLayout.PREFERRED_SIZE)
                .addContainerGap(162, Short.MAX_VALUE))
        );

        pnl_Content.addTab("Target", pnl_Target);

        pnl_Range.setLayout(new java.awt.BorderLayout());

        pnl_RangeForm.setBorder(javax.swing.BorderFactory.createEmptyBorder(10, 5, 10, 5));
        pnl_RangeForm.setLayout(new javax.swing.BoxLayout(pnl_RangeForm, javax.swing.BoxLayout.PAGE_AXIS));

        pnl_RangeRow1.setBorder(javax.swing.BorderFactory.createTitledBorder("X"));
        pnl_RangeRow1.setPreferredSize(new java.awt.Dimension(0, 60));

        btngrp_RangeX.add(btn_Range_X_Default);
        btn_Range_X_Default.setSelected(true);
        btn_Range_X_Default.setText("Default");
        btn_Range_X_Default.addActionListener(new java.awt.event.ActionListener() {
            @Override
            public void actionPerformed(java.awt.event.ActionEvent evt) {
                btn_Range_X_DefaultActionPerformed(evt);
            }
        });

        btngrp_RangeX.add(btn_Range_X_Specified);
        btn_Range_X_Specified.setText("Specified");
        btn_Range_X_Specified.addActionListener(new java.awt.event.ActionListener() {
            @Override
            public void actionPerformed(java.awt.event.ActionEvent evt) {
                btn_Range_X_SpecifiedActionPerformed(evt);
            }
        });

        javax.swing.GroupLayout pnl_RangeRow1Layout = new javax.swing.GroupLayout(pnl_RangeRow1);
        pnl_RangeRow1.setLayout(pnl_RangeRow1Layout);
        pnl_RangeRow1Layout.setHorizontalGroup(
            pnl_RangeRow1Layout.createParallelGroup(javax.swing.GroupLayout.Alignment.LEADING)
            .addGroup(pnl_RangeRow1Layout.createSequentialGroup()
                .addContainerGap()
                .addComponent(btn_Range_X_Default)
                .addGap(38, 38, 38)
                .addComponent(btn_Range_X_Specified)
                .addGap(71, 71, 71)
                .addComponent(spn_Range_X_Start, javax.swing.GroupLayout.PREFERRED_SIZE, 193, javax.swing.GroupLayout.PREFERRED_SIZE)
                .addGap(37, 37, 37)
                .addComponent(spn_Range_X_End, javax.swing.GroupLayout.PREFERRED_SIZE, 65, javax.swing.GroupLayout.PREFERRED_SIZE)
                .addContainerGap(78, Short.MAX_VALUE))
        );
        pnl_RangeRow1Layout.setVerticalGroup(
            pnl_RangeRow1Layout.createParallelGroup(javax.swing.GroupLayout.Alignment.LEADING)
            .addGroup(pnl_RangeRow1Layout.createSequentialGroup()
                .addGap(0, 0, 0)
                .addGroup(pnl_RangeRow1Layout.createParallelGroup(javax.swing.GroupLayout.Alignment.BASELINE)
                    .addComponent(btn_Range_X_Default)
                    .addComponent(btn_Range_X_Specified)
                    .addComponent(spn_Range_X_Start, javax.swing.GroupLayout.PREFERRED_SIZE, javax.swing.GroupLayout.DEFAULT_SIZE, javax.swing.GroupLayout.PREFERRED_SIZE)
                    .addComponent(spn_Range_X_End, javax.swing.GroupLayout.PREFERRED_SIZE, javax.swing.GroupLayout.DEFAULT_SIZE, javax.swing.GroupLayout.PREFERRED_SIZE))
                .addContainerGap())
        );

        pnl_RangeForm.add(pnl_RangeRow1);

        pnl_RangeRow2.setBorder(javax.swing.BorderFactory.createTitledBorder("Y1"));
        pnl_RangeRow2.setPreferredSize(new java.awt.Dimension(0, 60));

        btngrp_RangeY1.add(btn_Range_Y_Default);
        btn_Range_Y_Default.setSelected(true);
        btn_Range_Y_Default.setText("Default");
        btn_Range_Y_Default.addActionListener(new java.awt.event.ActionListener() {
            @Override
            public void actionPerformed(java.awt.event.ActionEvent evt) {
                btn_Range_Y_DefaultActionPerformed(evt);
            }
        });

        btngrp_RangeY1.add(btn_Range_Y_Specified);
        btn_Range_Y_Specified.setText("Specified");
        btn_Range_Y_Specified.addActionListener(new java.awt.event.ActionListener() {
            @Override
            public void actionPerformed(java.awt.event.ActionEvent evt) {
                btn_Range_Y_SpecifiedActionPerformed(evt);
            }
        });

        javax.swing.GroupLayout pnl_RangeRow2Layout = new javax.swing.GroupLayout(pnl_RangeRow2);
        pnl_RangeRow2.setLayout(pnl_RangeRow2Layout);
        pnl_RangeRow2Layout.setHorizontalGroup(
            pnl_RangeRow2Layout.createParallelGroup(javax.swing.GroupLayout.Alignment.LEADING)
            .addGroup(pnl_RangeRow2Layout.createSequentialGroup()
                .addContainerGap()
                .addComponent(btn_Range_Y_Default)
                .addGap(38, 38, 38)
                .addComponent(btn_Range_Y_Specified)
                .addGap(71, 71, 71)
                .addComponent(spn_Range_Y_Start, javax.swing.GroupLayout.PREFERRED_SIZE, 193, javax.swing.GroupLayout.PREFERRED_SIZE)
                .addGap(37, 37, 37)
                .addComponent(spn_Range_Y_End, javax.swing.GroupLayout.PREFERRED_SIZE, 65, javax.swing.GroupLayout.PREFERRED_SIZE)
                .addContainerGap(78, Short.MAX_VALUE))
        );
        pnl_RangeRow2Layout.setVerticalGroup(
            pnl_RangeRow2Layout.createParallelGroup(javax.swing.GroupLayout.Alignment.LEADING)
            .addGroup(javax.swing.GroupLayout.Alignment.TRAILING, pnl_RangeRow2Layout.createSequentialGroup()
                .addGap(0, 0, 0)
                .addGroup(pnl_RangeRow2Layout.createParallelGroup(javax.swing.GroupLayout.Alignment.BASELINE)
                    .addComponent(btn_Range_Y_Default)
                    .addComponent(btn_Range_Y_Specified)
                    .addComponent(spn_Range_Y_Start, javax.swing.GroupLayout.PREFERRED_SIZE, javax.swing.GroupLayout.DEFAULT_SIZE, javax.swing.GroupLayout.PREFERRED_SIZE)
                    .addComponent(spn_Range_Y_End, javax.swing.GroupLayout.PREFERRED_SIZE, javax.swing.GroupLayout.DEFAULT_SIZE, javax.swing.GroupLayout.PREFERRED_SIZE))
                .addContainerGap())
        );

        pnl_RangeForm.add(pnl_RangeRow2);

        pnl_RangeRow3.setBorder(javax.swing.BorderFactory.createTitledBorder("Y2"));
        pnl_RangeRow3.setPreferredSize(new java.awt.Dimension(0, 60));

        btngrp_RangeY2.add(btn_Range_Y2_Specified);
        btn_Range_Y2_Specified.setText("Specified");
        btn_Range_Y2_Specified.addActionListener(new java.awt.event.ActionListener() {
            @Override
            public void actionPerformed(java.awt.event.ActionEvent evt) {
                btn_Range_Y2_SpecifiedActionPerformed(evt);
            }
        });

        btngrp_RangeY2.add(btn_Range_Y2_Default);
        btn_Range_Y2_Default.setSelected(true);
        btn_Range_Y2_Default.setText("Default");
        btn_Range_Y2_Default.addActionListener(new java.awt.event.ActionListener() {
            @Override
            public void actionPerformed(java.awt.event.ActionEvent evt) {
                btn_Range_Y2_DefaultActionPerformed(evt);
            }
        });

        javax.swing.GroupLayout pnl_RangeRow3Layout = new javax.swing.GroupLayout(pnl_RangeRow3);
        pnl_RangeRow3.setLayout(pnl_RangeRow3Layout);
        pnl_RangeRow3Layout.setHorizontalGroup(
            pnl_RangeRow3Layout.createParallelGroup(javax.swing.GroupLayout.Alignment.LEADING)
            .addGroup(pnl_RangeRow3Layout.createSequentialGroup()
                .addContainerGap()
                .addComponent(btn_Range_Y2_Default)
                .addGap(38, 38, 38)
                .addComponent(btn_Range_Y2_Specified)
                .addGap(71, 71, 71)
                .addComponent(spn_Range_Y2_Start, javax.swing.GroupLayout.PREFERRED_SIZE, 193, javax.swing.GroupLayout.PREFERRED_SIZE)
                .addGap(37, 37, 37)
                .addComponent(spn_Range_Y2_End, javax.swing.GroupLayout.PREFERRED_SIZE, 65, javax.swing.GroupLayout.PREFERRED_SIZE)
                .addContainerGap(78, Short.MAX_VALUE))
        );
        pnl_RangeRow3Layout.setVerticalGroup(
            pnl_RangeRow3Layout.createParallelGroup(javax.swing.GroupLayout.Alignment.LEADING)
            .addGroup(pnl_RangeRow3Layout.createSequentialGroup()
                .addGap(0, 0, 0)
                .addGroup(pnl_RangeRow3Layout.createParallelGroup(javax.swing.GroupLayout.Alignment.BASELINE)
                    .addComponent(btn_Range_Y2_Default)
                    .addComponent(btn_Range_Y2_Specified)
                    .addComponent(spn_Range_Y2_Start, javax.swing.GroupLayout.PREFERRED_SIZE, javax.swing.GroupLayout.DEFAULT_SIZE, javax.swing.GroupLayout.PREFERRED_SIZE)
                    .addComponent(spn_Range_Y2_End, javax.swing.GroupLayout.PREFERRED_SIZE, javax.swing.GroupLayout.DEFAULT_SIZE, javax.swing.GroupLayout.PREFERRED_SIZE))
                .addContainerGap())
        );

        pnl_RangeForm.add(pnl_RangeRow3);

        pnl_Range.add(pnl_RangeForm, java.awt.BorderLayout.CENTER);

        javax.swing.GroupLayout pnl_RangeMarginBottomLayout = new javax.swing.GroupLayout(pnl_RangeMarginBottom);
        pnl_RangeMarginBottom.setLayout(pnl_RangeMarginBottomLayout);
        pnl_RangeMarginBottomLayout.setHorizontalGroup(
            pnl_RangeMarginBottomLayout.createParallelGroup(javax.swing.GroupLayout.Alignment.LEADING)
            .addGap(0, 692, Short.MAX_VALUE)
        );
        pnl_RangeMarginBottomLayout.setVerticalGroup(
            pnl_RangeMarginBottomLayout.createParallelGroup(javax.swing.GroupLayout.Alignment.LEADING)
            .addGap(0, 100, Short.MAX_VALUE)
        );

        pnl_Range.add(pnl_RangeMarginBottom, java.awt.BorderLayout.SOUTH);

        pnl_Content.addTab("Range", pnl_Range);

        pnl_Legend.setLayout(new java.awt.BorderLayout());

        pnl_LegendTop.setBorder(javax.swing.BorderFactory.createEmptyBorder(0, 5, 0, 5));
        pnl_LegendTop.setMinimumSize(new java.awt.Dimension(0, 0));
        pnl_LegendTop.setPreferredSize(new java.awt.Dimension(30, 30));
        pnl_LegendTop.setLayout(new java.awt.BorderLayout(10, 0));

        chk_Disp_Introductory.setSelected(true);
        chk_Disp_Introductory.setText("Title:");
        chk_Disp_Introductory.setAlignmentX(0.5F);
        chk_Disp_Introductory.setHorizontalTextPosition(javax.swing.SwingConstants.LEFT);
        chk_Disp_Introductory.setIconTextGap(15);
        chk_Disp_Introductory.setPreferredSize(new java.awt.Dimension(104, 30));
        chk_Disp_Introductory.setVerticalTextPosition(javax.swing.SwingConstants.BOTTOM);
        pnl_LegendTop.add(chk_Disp_Introductory, java.awt.BorderLayout.SOUTH);

        pnl_Legend.add(pnl_LegendTop, java.awt.BorderLayout.NORTH);

        pnl_LegendCenter.setBorder(javax.swing.BorderFactory.createEmptyBorder(0, 5, 10, 5));
        pnl_LegendCenter.setPreferredSize(new java.awt.Dimension(672, 240));
        pnl_LegendCenter.setLayout(new javax.swing.BoxLayout(pnl_LegendCenter, javax.swing.BoxLayout.PAGE_AXIS));

        pnl_LegendRow1.setBorder(javax.swing.BorderFactory.createTitledBorder("X"));
        pnl_LegendRow1.setPreferredSize(new java.awt.Dimension(0, 60));

        btngrp_LegendX.add(btn_Legend_X_Default);
        btn_Legend_X_Default.setSelected(true);
        btn_Legend_X_Default.setText("Default");
        btn_Legend_X_Default.addActionListener(new java.awt.event.ActionListener() {
            @Override
            public void actionPerformed(java.awt.event.ActionEvent evt) {
                btn_Legend_X_DefaultActionPerformed(evt);
            }
        });

        btngrp_LegendX.add(btn_Legend_X_Specified);
        btn_Legend_X_Specified.setText("Specified");
        btn_Legend_X_Specified.addActionListener(new java.awt.event.ActionListener() {
            @Override
            public void actionPerformed(java.awt.event.ActionEvent evt) {
                btn_Legend_X_SpecifiedActionPerformed(evt);
            }
        });

        txt_Legend_X.setFont(new java.awt.Font("Dialog", 0, 13)); // NOI18N

        javax.swing.GroupLayout pnl_LegendRow1Layout = new javax.swing.GroupLayout(pnl_LegendRow1);
        pnl_LegendRow1.setLayout(pnl_LegendRow1Layout);
        pnl_LegendRow1Layout.setHorizontalGroup(
            pnl_LegendRow1Layout.createParallelGroup(javax.swing.GroupLayout.Alignment.LEADING)
            .addGroup(pnl_LegendRow1Layout.createSequentialGroup()
                .addContainerGap()
                .addComponent(btn_Legend_X_Default)
                .addGap(37, 37, 37)
                .addComponent(btn_Legend_X_Specified)
                .addGap(37, 37, 37)
                .addComponent(txt_Legend_X, javax.swing.GroupLayout.DEFAULT_SIZE, 396, Short.MAX_VALUE)
                .addContainerGap())
        );
        pnl_LegendRow1Layout.setVerticalGroup(
            pnl_LegendRow1Layout.createParallelGroup(javax.swing.GroupLayout.Alignment.LEADING)
            .addGroup(pnl_LegendRow1Layout.createSequentialGroup()
                .addGroup(pnl_LegendRow1Layout.createParallelGroup(javax.swing.GroupLayout.Alignment.BASELINE)
                    .addComponent(btn_Legend_X_Default)
                    .addComponent(btn_Legend_X_Specified)
                    .addComponent(txt_Legend_X, javax.swing.GroupLayout.PREFERRED_SIZE, javax.swing.GroupLayout.DEFAULT_SIZE, javax.swing.GroupLayout.PREFERRED_SIZE))
                .addContainerGap())
        );

        pnl_LegendCenter.add(pnl_LegendRow1);

        pnl_LegendRow2.setBorder(javax.swing.BorderFactory.createTitledBorder("Y"));
        pnl_LegendRow2.setPreferredSize(new java.awt.Dimension(0, 60));

        btngrp_LegendY1.add(btn_Legend_Y_Specified);
        btn_Legend_Y_Specified.setText("Specified");
        btn_Legend_Y_Specified.addActionListener(new java.awt.event.ActionListener() {
            @Override
            public void actionPerformed(java.awt.event.ActionEvent evt) {
                btn_Legend_Y_SpecifiedActionPerformed(evt);
            }
        });

        btngrp_LegendY1.add(btn_Legend_Y_Default);
        btn_Legend_Y_Default.setSelected(true);
        btn_Legend_Y_Default.setText("Default");
        btn_Legend_Y_Default.addActionListener(new java.awt.event.ActionListener() {
            @Override
            public void actionPerformed(java.awt.event.ActionEvent evt) {
                btn_Legend_Y_DefaultActionPerformed(evt);
            }
        });

        txt_Legend_Y.setFont(new java.awt.Font("Dialog", 0, 13)); // NOI18N

        javax.swing.GroupLayout pnl_LegendRow2Layout = new javax.swing.GroupLayout(pnl_LegendRow2);
        pnl_LegendRow2.setLayout(pnl_LegendRow2Layout);
        pnl_LegendRow2Layout.setHorizontalGroup(
            pnl_LegendRow2Layout.createParallelGroup(javax.swing.GroupLayout.Alignment.LEADING)
            .addGroup(pnl_LegendRow2Layout.createSequentialGroup()
                .addContainerGap()
                .addComponent(btn_Legend_Y_Default)
                .addGap(37, 37, 37)
                .addComponent(btn_Legend_Y_Specified)
                .addGap(37, 37, 37)
                .addComponent(txt_Legend_Y, javax.swing.GroupLayout.DEFAULT_SIZE, 396, Short.MAX_VALUE)
                .addContainerGap())
        );
        pnl_LegendRow2Layout.setVerticalGroup(
            pnl_LegendRow2Layout.createParallelGroup(javax.swing.GroupLayout.Alignment.LEADING)
            .addGroup(pnl_LegendRow2Layout.createSequentialGroup()
                .addGap(0, 0, 0)
                .addGroup(pnl_LegendRow2Layout.createParallelGroup(javax.swing.GroupLayout.Alignment.BASELINE)
                    .addComponent(btn_Legend_Y_Default)
                    .addComponent(btn_Legend_Y_Specified)
                    .addComponent(txt_Legend_Y, javax.swing.GroupLayout.PREFERRED_SIZE, javax.swing.GroupLayout.DEFAULT_SIZE, javax.swing.GroupLayout.PREFERRED_SIZE))
                .addContainerGap())
        );

        pnl_LegendCenter.add(pnl_LegendRow2);

        pnl_LegendRow3.setBorder(javax.swing.BorderFactory.createTitledBorder("Y2"));
        pnl_LegendRow3.setPreferredSize(new java.awt.Dimension(0, 60));

        btngrp_LegendY2.add(btn_Legend_Y2_Default);
        btn_Legend_Y2_Default.setSelected(true);
        btn_Legend_Y2_Default.setText("Default");
        btn_Legend_Y2_Default.addActionListener(new java.awt.event.ActionListener() {
            @Override
            public void actionPerformed(java.awt.event.ActionEvent evt) {
                btn_Legend_Y2_DefaultActionPerformed(evt);
            }
        });

        btngrp_LegendY2.add(btn_Legend_Y2_Specified);
        btn_Legend_Y2_Specified.setText("Specified");
        btn_Legend_Y2_Specified.addActionListener(new java.awt.event.ActionListener() {
            @Override
            public void actionPerformed(java.awt.event.ActionEvent evt) {
                btn_Legend_Y2_SpecifiedActionPerformed(evt);
            }
        });

        txt_Legend_Y2.setFont(new java.awt.Font("Dialog", 0, 13)); // NOI18N

        javax.swing.GroupLayout pnl_LegendRow3Layout = new javax.swing.GroupLayout(pnl_LegendRow3);
        pnl_LegendRow3.setLayout(pnl_LegendRow3Layout);
        pnl_LegendRow3Layout.setHorizontalGroup(
            pnl_LegendRow3Layout.createParallelGroup(javax.swing.GroupLayout.Alignment.LEADING)
            .addGroup(pnl_LegendRow3Layout.createSequentialGroup()
                .addContainerGap()
                .addComponent(btn_Legend_Y2_Default)
                .addGap(37, 37, 37)
                .addComponent(btn_Legend_Y2_Specified)
                .addGap(37, 37, 37)
                .addComponent(txt_Legend_Y2, javax.swing.GroupLayout.DEFAULT_SIZE, 396, Short.MAX_VALUE)
                .addContainerGap())
        );
        pnl_LegendRow3Layout.setVerticalGroup(
            pnl_LegendRow3Layout.createParallelGroup(javax.swing.GroupLayout.Alignment.LEADING)
            .addGroup(pnl_LegendRow3Layout.createSequentialGroup()
                .addGroup(pnl_LegendRow3Layout.createParallelGroup(javax.swing.GroupLayout.Alignment.BASELINE)
                    .addComponent(btn_Legend_Y2_Default)
                    .addComponent(btn_Legend_Y2_Specified)
                    .addComponent(txt_Legend_Y2, javax.swing.GroupLayout.PREFERRED_SIZE, javax.swing.GroupLayout.DEFAULT_SIZE, javax.swing.GroupLayout.PREFERRED_SIZE))
                .addContainerGap())
        );

        pnl_LegendCenter.add(pnl_LegendRow3);

        pnl_Legend.add(pnl_LegendCenter, java.awt.BorderLayout.CENTER);

        javax.swing.GroupLayout pnl_LegendMarginBottomLayout = new javax.swing.GroupLayout(pnl_LegendMarginBottom);
        pnl_LegendMarginBottom.setLayout(pnl_LegendMarginBottomLayout);
        pnl_LegendMarginBottomLayout.setHorizontalGroup(
            pnl_LegendMarginBottomLayout.createParallelGroup(javax.swing.GroupLayout.Alignment.LEADING)
            .addGap(0, 692, Short.MAX_VALUE)
        );
        pnl_LegendMarginBottomLayout.setVerticalGroup(
            pnl_LegendMarginBottomLayout.createParallelGroup(javax.swing.GroupLayout.Alignment.LEADING)
            .addGap(0, 100, Short.MAX_VALUE)
        );

        pnl_Legend.add(pnl_LegendMarginBottom, java.awt.BorderLayout.SOUTH);

        pnl_Content.addTab("Legend", pnl_Legend);

        pnl_Wrapper.add(pnl_Content, java.awt.BorderLayout.CENTER);

        pnl_Foot.setBorder(javax.swing.BorderFactory.createEmptyBorder(5, 15, 5, 15));
        pnl_Foot.setMinimumSize(new java.awt.Dimension(680, 40));
        pnl_Foot.setName(""); // NOI18N
        pnl_Foot.setPreferredSize(new java.awt.Dimension(680, 40));
        pnl_Foot.setLayout(new javax.swing.BoxLayout(pnl_Foot, javax.swing.BoxLayout.LINE_AXIS));

        btn_Plot.setText("Plot");
        btn_Plot.setMaximumSize(new java.awt.Dimension(32767, 23));
        btn_Plot.addActionListener(new java.awt.event.ActionListener() {
            @Override
            public void actionPerformed(java.awt.event.ActionEvent evt) {
                btn_PlotActionPerformed(evt);
            }
        });
        pnl_Foot.add(btn_Plot);

        pnl_Wrapper.add(pnl_Foot, java.awt.BorderLayout.SOUTH);

        getContentPane().add(pnl_Wrapper, java.awt.BorderLayout.CENTER);

        panelStatusBar.setBorder(javax.swing.BorderFactory.createEtchedBorder());

        lbl_trackInfo.setOpaque(true);

        javax.swing.GroupLayout panelStatusBarLayout = new javax.swing.GroupLayout(panelStatusBar);
        panelStatusBar.setLayout(panelStatusBarLayout);
        panelStatusBarLayout.setHorizontalGroup(
            panelStatusBarLayout.createParallelGroup(javax.swing.GroupLayout.Alignment.LEADING)
            .addGap(0, 700, Short.MAX_VALUE)
            .addGroup(panelStatusBarLayout.createParallelGroup(javax.swing.GroupLayout.Alignment.LEADING)
                .addComponent(lbl_trackInfo, javax.swing.GroupLayout.DEFAULT_SIZE, 700, Short.MAX_VALUE))
        );
        panelStatusBarLayout.setVerticalGroup(
            panelStatusBarLayout.createParallelGroup(javax.swing.GroupLayout.Alignment.LEADING)
            .addGap(0, 20, Short.MAX_VALUE)
            .addGroup(panelStatusBarLayout.createParallelGroup(javax.swing.GroupLayout.Alignment.LEADING)
                .addComponent(lbl_trackInfo, javax.swing.GroupLayout.DEFAULT_SIZE, 20, Short.MAX_VALUE))
        );

        getContentPane().add(panelStatusBar, java.awt.BorderLayout.SOUTH);

        pack();
    }// </editor-fold>//GEN-END:initComponents

    private void radio_OutputWindowActionPerformed(java.awt.event.ActionEvent evt) {//GEN-FIRST:event_radio_OutputWindowActionPerformed
        chgOutPutFilePanel();
    }//GEN-LAST:event_radio_OutputWindowActionPerformed

    private void radio_OutputFileActionPerformed(java.awt.event.ActionEvent evt) {//GEN-FIRST:event_radio_OutputFileActionPerformed
        chgOutPutFilePanel();
    }//GEN-LAST:event_radio_OutputFileActionPerformed

    private void btn_Range_X_DefaultActionPerformed(java.awt.event.ActionEvent evt) {//GEN-FIRST:event_btn_Range_X_DefaultActionPerformed
        chgRangeX();
    }//GEN-LAST:event_btn_Range_X_DefaultActionPerformed

    private void btn_Range_X_SpecifiedActionPerformed(java.awt.event.ActionEvent evt) {//GEN-FIRST:event_btn_Range_X_SpecifiedActionPerformed
        chgRangeX();
    }//GEN-LAST:event_btn_Range_X_SpecifiedActionPerformed

    private void btn_Range_Y_DefaultActionPerformed(java.awt.event.ActionEvent evt) {//GEN-FIRST:event_btn_Range_Y_DefaultActionPerformed
        chgRangeY();
    }//GEN-LAST:event_btn_Range_Y_DefaultActionPerformed

    private void btn_Range_Y_SpecifiedActionPerformed(java.awt.event.ActionEvent evt) {//GEN-FIRST:event_btn_Range_Y_SpecifiedActionPerformed
        chgRangeY();
    }//GEN-LAST:event_btn_Range_Y_SpecifiedActionPerformed

    private void btn_Range_Y2_SpecifiedActionPerformed(java.awt.event.ActionEvent evt) {//GEN-FIRST:event_btn_Range_Y2_SpecifiedActionPerformed
        chgRangeY2();
    }//GEN-LAST:event_btn_Range_Y2_SpecifiedActionPerformed

    private void btn_Range_Y2_DefaultActionPerformed(java.awt.event.ActionEvent evt) {//GEN-FIRST:event_btn_Range_Y2_DefaultActionPerformed
        chgRangeY2();
    }//GEN-LAST:event_btn_Range_Y2_DefaultActionPerformed

    private void btn_Legend_X_DefaultActionPerformed(java.awt.event.ActionEvent evt) {//GEN-FIRST:event_btn_Legend_X_DefaultActionPerformed
        chgLegendX();
    }//GEN-LAST:event_btn_Legend_X_DefaultActionPerformed

    private void btn_Legend_X_SpecifiedActionPerformed(java.awt.event.ActionEvent evt) {//GEN-FIRST:event_btn_Legend_X_SpecifiedActionPerformed
        chgLegendX();
    }//GEN-LAST:event_btn_Legend_X_SpecifiedActionPerformed

    private void btn_Legend_Y_SpecifiedActionPerformed(java.awt.event.ActionEvent evt) {//GEN-FIRST:event_btn_Legend_Y_SpecifiedActionPerformed
        chgLegendY();
    }//GEN-LAST:event_btn_Legend_Y_SpecifiedActionPerformed

    private void btn_Legend_Y_DefaultActionPerformed(java.awt.event.ActionEvent evt) {//GEN-FIRST:event_btn_Legend_Y_DefaultActionPerformed
        chgLegendY();
    }//GEN-LAST:event_btn_Legend_Y_DefaultActionPerformed

    private void btn_Legend_Y2_DefaultActionPerformed(java.awt.event.ActionEvent evt) {//GEN-FIRST:event_btn_Legend_Y2_DefaultActionPerformed
        chgLegendY2();
    }//GEN-LAST:event_btn_Legend_Y2_DefaultActionPerformed

    private void btn_Legend_Y2_SpecifiedActionPerformed(java.awt.event.ActionEvent evt) {//GEN-FIRST:event_btn_Legend_Y2_SpecifiedActionPerformed
        chgLegendY2();
    }//GEN-LAST:event_btn_Legend_Y2_SpecifiedActionPerformed

    private void btn_PlotActionPerformed(java.awt.event.ActionEvent evt) {//GEN-FIRST:event_btn_PlotActionPerformed
        PlotterLoader loader = new PlotterLoader();
        setExportCsvAndPlotEnabled(false);
        try {
            final IPlotter plotter = loader.loadDefault();
            final SwingWorker worker = plotter.getWorker(this);

            final File outputFile = new File(txt_OutputFilePath.getText());
            GnuPlotter.TerminalType terminalType = 
                    GnuPlotter.TerminalType.valueOf(getTerminalType());

            if (!GnuPlotter.TerminalType.WINDOW.equals(terminalType) && outputFile.exists()) {
                int ans = JOptionPane.showConfirmDialog(this,
                                            "Is it OK to replace the existing file?",
                                            "Replace the existing file?",
                                                            JOptionPane.YES_NO_OPTION);
                if (ans != JOptionPane.YES_OPTION) return;
            }

            worker.addPropertyChangeListener(new PropertyChangeListener () {
                @Override
                public void propertyChange(PropertyChangeEvent evt) {
                    String propertyName = evt.getPropertyName();
                    Object nv = evt.getNewValue();
                    if ("state".equals(propertyName)
                        && nv == SwingWorker.StateValue.DONE) {
                        setExportCsvAndPlotEnabled(true);
                        try {
                            worker.get();

                            GnuPlotter.TerminalType terminalType = 
                                    GnuPlotter.TerminalType.valueOf(getTerminalType());

                            if (!GnuPlotter.TerminalType.WINDOW.equals(terminalType)) {
                                String title = "Save plot file.";
                                String msg = "Save to  " + outputFile.getName();
                                JOptionPane.showMessageDialog(PlotWindow.this, msg, title, 
                                        JOptionPane.INFORMATION_MESSAGE);
                            }
                        } catch (InterruptedException ex) {
                        } catch (ExecutionException ex) {
                            String title = "Error while executing the Plotter";
                            String msg = ex.getMessage();
                            if (ex.getCause() != null)
                                msg = ex.getCause().getMessage();

                            JOptionPane optionPane = new JOptionPane(msg, JOptionPane.ERROR_MESSAGE);
                            JDialog dialog = optionPane.createDialog(title);
                            dialog.setModalityType(JDialog.ModalityType.APPLICATION_MODAL);

                            dialog.setVisible(true);
                        }
                    }    
                }
            });
            worker.execute();
        } catch (PlotterLoadException | HeadlessException ex) {
            showErrorDialog(ex.getMessage(), "Error on plotting");
            setExportCsvAndPlotEnabled(true);
        }
    }//GEN-LAST:event_btn_PlotActionPerformed

    private void btn_OutputFilePathActionPerformed(java.awt.event.ActionEvent evt) {//GEN-FIRST:event_btn_OutputFilePathActionPerformed
        String homePath = System.getProperty("user.home");
        String fileName = Utility.getFileName(mModelFile.getPath());

        String extension = "";
        GnuPlotter.TerminalType terminalType = GnuPlotter.TerminalType.valueOf(getTerminalType());

        if (!GnuPlotter.TerminalType.WINDOW.equals(terminalType))
            extension = "." + terminalType.name().toLowerCase(Locale.ENGLISH);

        String filePath = txt_OutputFilePath.getText();

        File defaultFile;
        if (filePath == null || filePath.isEmpty()) {
            defaultFile = new File(homePath, fileName + "_" + mJob.getJobId() + extension);
        } else {
            defaultFile = new File(filePath);
        }
        FileChooser fc = new FileChooser(this, "Select output file", FileChooser.Mode.SAVE, defaultFile);

        if (fc.showDialog()) {
            txt_OutputFilePath.setText(fc.getSelectedFile().getPath());
        }
    }//GEN-LAST:event_btn_OutputFilePathActionPerformed

    private void checkBoxY2logActionPerformed(java.awt.event.ActionEvent evt) {//GEN-FIRST:event_checkBoxY2logActionPerformed
        renderPlot();
    }//GEN-LAST:event_checkBoxY2logActionPerformed

    private void checkBoxY1logActionPerformed(java.awt.event.ActionEvent evt) {//GEN-FIRST:event_checkBoxY1logActionPerformed
        renderPlot();
    }//GEN-LAST:event_checkBoxY1logActionPerformed

    private void checkBoxLegendActionPerformed(java.awt.event.ActionEvent evt) {//GEN-FIRST:event_checkBoxLegendActionPerformed
        renderPlot();
    }//GEN-LAST:event_checkBoxLegendActionPerformed

    private void checkBoxXlogActionPerformed(java.awt.event.ActionEvent evt) {//GEN-FIRST:event_checkBoxXlogActionPerformed
        renderPlot();
    }//GEN-LAST:event_checkBoxXlogActionPerformed

    private void showErrorDialog (String msg, String title) {
        JOptionPane.showMessageDialog(this, msg, title, JOptionPane.ERROR_MESSAGE);
    }

    // Variables declaration - do not modify//GEN-BEGIN:variables
    private javax.swing.JRadioButton btn_Legend_X_Default;
    private javax.swing.JRadioButton btn_Legend_X_Specified;
    private javax.swing.JRadioButton btn_Legend_Y2_Default;
    private javax.swing.JRadioButton btn_Legend_Y2_Specified;
    private javax.swing.JRadioButton btn_Legend_Y_Default;
    private javax.swing.JRadioButton btn_Legend_Y_Specified;
    private javax.swing.JButton btn_OutputFilePath;
    private javax.swing.JButton btn_Plot;
    private javax.swing.JRadioButton btn_Range_X_Default;
    private javax.swing.JRadioButton btn_Range_X_Specified;
    private javax.swing.JRadioButton btn_Range_Y2_Default;
    private javax.swing.JRadioButton btn_Range_Y2_Specified;
    private javax.swing.JRadioButton btn_Range_Y_Default;
    private javax.swing.JRadioButton btn_Range_Y_Specified;
    private javax.swing.ButtonGroup btngrp_LegendX;
    private javax.swing.ButtonGroup btngrp_LegendY1;
    private javax.swing.ButtonGroup btngrp_LegendY2;
    private javax.swing.ButtonGroup btngrp_RangeX;
    private javax.swing.ButtonGroup btngrp_RangeY1;
    private javax.swing.ButtonGroup btngrp_RangeY2;
    private javax.swing.ButtonGroup btngrp_Target;
    private javax.swing.JCheckBox checkBoxLegend;
    private javax.swing.JCheckBox checkBoxXlog;
    private javax.swing.JCheckBox checkBoxY1log;
    private javax.swing.JCheckBox checkBoxY2log;
    private javax.swing.JCheckBox chk_Disp_Introductory;
    private javax.swing.JComboBox cmb_TerminalType;
    private javax.swing.JPanel jPanel1;
    private javax.swing.JPanel jPanel2;
    private javax.swing.JSplitPane jSplitPane1;
    private javax.swing.JScrollPane js_VariableList;
    private javax.swing.JScrollPane js_VariableXList;
    private javax.swing.JScrollPane js_VariableY2List;
    private javax.swing.JScrollPane js_VariableYList;
    private javax.swing.JLabel lbl_Abscissa;
    private javax.swing.JLabel lbl_Format;
    private javax.swing.JLabel lbl_Path;
    private javax.swing.JLabel lbl_Variables;
    private javax.swing.JLabel lbl_Y1;
    private javax.swing.JLabel lbl_Y2;
    private javax.swing.JLabel lbl_trackInfo;
    private javax.swing.JPanel panelStatusBar;
    private javax.swing.JTabbedPane pnl_Content;
    private javax.swing.JPanel pnl_Foot;
    private javax.swing.JPanel pnl_Legend;
    private javax.swing.JPanel pnl_LegendCenter;
    private javax.swing.JPanel pnl_LegendMarginBottom;
    private javax.swing.JPanel pnl_LegendRow1;
    private javax.swing.JPanel pnl_LegendRow2;
    private javax.swing.JPanel pnl_LegendRow3;
    private javax.swing.JPanel pnl_LegendTop;
    private javax.swing.JPanel pnl_OutputFile;
    private javax.swing.JPanel pnl_PlotArea;
    private javax.swing.JPanel pnl_Range;
    private javax.swing.JPanel pnl_RangeForm;
    private javax.swing.JPanel pnl_RangeMarginBottom;
    private javax.swing.JPanel pnl_RangeRow1;
    private javax.swing.JPanel pnl_RangeRow2;
    private javax.swing.JPanel pnl_RangeRow3;
    private javax.swing.JPanel pnl_Target;
    private javax.swing.JPanel pnl_Track;
    private javax.swing.JPanel pnl_Wrapper;
    private javax.swing.JRadioButton radio_OutputFile;
    private javax.swing.JRadioButton radio_OutputWindow;
    private javax.swing.JSpinner spn_Range_X_End;
    private javax.swing.JSpinner spn_Range_X_Start;
    private javax.swing.JSpinner spn_Range_Y2_End;
    private javax.swing.JSpinner spn_Range_Y2_Start;
    private javax.swing.JSpinner spn_Range_Y_End;
    private javax.swing.JSpinner spn_Range_Y_Start;
    private javax.swing.JTextField txt_Legend_X;
    private javax.swing.JTextField txt_Legend_Y;
    private javax.swing.JTextField txt_Legend_Y2;
    private javax.swing.JTextField txt_OutputFilePath;
    // End of variables declaration//GEN-END:variables
}
