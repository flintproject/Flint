/* -*- Mode: Java; tab-width: 4; indent-tabs-mode: nil; c-basic-offset: 4 -*- vim:set ts=4 sw=4 sts=4 et: */
package jp.oist.flint.form.sub;

import jp.oist.flint.executor.ModelReloader;
import java.awt.Component;
import java.awt.Dimension;
import java.awt.KeyboardFocusManager;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.awt.event.WindowAdapter;
import java.awt.event.WindowEvent;
import java.io.File;
import java.io.IOException;
import java.text.SimpleDateFormat;
import java.util.ArrayList;
import java.util.Date;
import java.util.Vector;
import java.util.concurrent.ExecutionException;
import java.util.concurrent.locks.Lock;
import java.util.concurrent.locks.ReentrantLock;
import javax.swing.JButton;
import javax.swing.JFrame;
import javax.swing.JOptionPane;
import javax.swing.JTabbedPane;
import javax.swing.JInternalFrame;
import com.google.protobuf.ByteString;
import java.beans.PropertyChangeEvent;
import java.beans.PropertyChangeListener;
import java.beans.PropertyVetoException;
import java.util.Arrays;
import java.util.logging.Level;
import java.util.logging.Logger;
import javax.swing.JComponent;
import javax.xml.parsers.ParserConfigurationException;
import jp.oist.flint.dao.TaskDao;
import jp.oist.flint.backend.ModelLoader;
import jp.oist.flint.executor.PhspSimulator;
import jp.oist.flint.filesystem.IModelFileClient;
import jp.oist.flint.form.IFrame;
import jp.oist.flint.form.job.IProgressManager;
import jp.oist.flint.form.MainFrame;
import jp.oist.flint.form.log.LogWindow;
import jp.oist.flint.form.ProgressPane;
import jp.oist.flint.k3.K3Loader;
import jp.oist.flint.phsp.entity.Model;
import jp.oist.flint.phsp.entity.ParameterSet;
import jp.oist.flint.phsp.entity.TargetSet;
import jp.oist.flint.sedml.ISimulationConfiguration;
import jp.physiome.Ipc;

public class SubFrame extends JInternalFrame
    implements ActionListener, IModelFileClient,
        ISimulationConfiguration, IFrame,
        PhspSimulator.Listener {

    public final static String ID = "flint.view.sub";

    private final static String LABEL_SEPARATOR = "@";

    private final MainFrame mMainFrame;
    private final File mOriginalFile;
    private final String mOriginalPath;
    private Ipc.ModelProbeResponse mModelProbeResponse;

    private jp.oist.flint.backend.ModelLoader mLoader;
    private final K3Loader mK3Loader;

    /**
     * The status bar of a form.
     */
    private JComponent mStatusComponent;

    private boolean mIsEditable = true;

    private final Lock mLock = new ReentrantLock();

    private LogWindow mLogWindow = null;
    private JobWindow mJobWindow = null;

    // Swing components
    private JTabbedPane pnl_Content;

    private GeneralSettingPane pnl_GeneralSetting;

    private VariableSelectionPane pnl_VariableSelection;

    private ParameterValuePane pnl_ParameterValue;
    private String mRelativePath;

    private PhspSimulator mSimulator = null;

    private SelectionHandler mSelectionHandler;

    public SubFrame(MainFrame mainFrame, Model model, ModelLoader loader)
            throws IOException, ExecutionException, InterruptedException {
        this(mainFrame, model.getModelFile(), loader);

        loadParameterAndTarget(model);
    }

    static int mAutoIncrement = 0;

    public SubFrame(MainFrame mainFrame, File file, ModelLoader loader)
            throws IOException, ExecutionException, InterruptedException {
        super(file.getAbsolutePath(), true, true, true, true);

        mMainFrame = mainFrame;
        mOriginalFile = file;
        mOriginalPath = file.getCanonicalPath();
        mRelativePath = file.getPath();
        mLoader = loader;
        mK3Loader = new K3Loader();

        init(file, loader);

        mSelectionHandler = new SelectionHandler(this);
        addPropertyChangeListener(mSelectionHandler);
    }

    private void init(File file, ModelLoader loader)
            throws IOException, ExecutionException, InterruptedException {

        mLoader = loader;
        mModelProbeResponse = loader.get();
        Ipc.ModelProbeResponse response = loader.get();

        initComponents();

        initSubWindow();

        String tmpIntegrationMethod = response.getIntegrationMethod().toString();
        String[] integrationMethodArr = tmpIntegrationMethod.split("_");
        String integrationMethod = "";

        if (integrationMethodArr.length > 1) {
            for (String str : integrationMethodArr) {
                str = str.substring(0, 1).toUpperCase() + str.substring(1).toLowerCase();
                integrationMethod += str + "-";
            }
            int index = integrationMethod.lastIndexOf("-");
            integrationMethod = integrationMethod.substring(0, index);
        } else {
            integrationMethod = tmpIntegrationMethod.substring(0, 1).toUpperCase()
                    + tmpIntegrationMethod.substring(1).toLowerCase();
        }
        pnl_GeneralSetting.setSelectedIntegrationMethod(integrationMethod);
        pnl_GeneralSetting.setSimulationLegnth(response.getLength());
        pnl_GeneralSetting.setSimulationStep(response.getStep());

        int timeUnitCount = response.getTimeUnitCount();
        Vector<String> options = new Vector<>(timeUnitCount);

        for (int i = 0; i < timeUnitCount; i++) {
            options.addElement(response.getTimeUnit(i).getName());
        }

        pnl_GeneralSetting.setInitializedLengthUnitItems(options);
        pnl_GeneralSetting.setSelectedLengthUnit(response.getLengthUnit().getName());

        pnl_GeneralSetting.setInitializedStepUnitItems(options);
        pnl_GeneralSetting.setSelectedStepUnit(response.getStepUnit().getName());

        pnl_VariableSelection.setSelectedFilterSyntax(1);

        setVisible(true);
    }

    private void initComponents()
            throws ExecutionException, InterruptedException, IOException {

        Dimension dim = new Dimension(600, 400);

        setMaximumSize(new Dimension(Short.MAX_VALUE, Short.MAX_VALUE));
        setMinimumSize(dim);
        setPreferredSize(dim);
        setSize(dim);

        pnl_Content = new JTabbedPane();
        pnl_Content.setOpaque(true);
        setContentPane(pnl_Content);

        pnl_GeneralSetting = new GeneralSettingPane(mOriginalFile, mLoader);
        pnl_Content.addTab("General Settings", pnl_GeneralSetting);

        pnl_VariableSelection = new VariableSelectionPane(mOriginalFile, mLoader);
        pnl_Content.addTab("Output Variables", pnl_VariableSelection);

        pnl_ParameterValue = new ParameterValuePane(mOriginalFile, mLoader);
        pnl_Content.addTab("Parameters", pnl_ParameterValue);

        mStatusComponent = null;
    }

    private void initSubWindow()
            throws IOException {
        mLogWindow = new LogWindow(null, String.format("Log [%s]",
                mOriginalPath));
        Dimension wsize = new Dimension(600, 400);
        mLogWindow.setPreferredSize(wsize);
        mLogWindow.setSize(wsize);
        mLogWindow.setMinimumSize(wsize);
        mLogWindow.setDefaultCloseOperation(JFrame.DO_NOTHING_ON_CLOSE);
        mLogWindow.setLocationRelativeTo(this);
        mLogWindow.addWindowListener(new WindowAdapter() {
            @Override
            public void windowClosing(WindowEvent e) {
                mLogWindow.setVisible(false);
            }
        });
        mLogWindow.pack();
        mLogWindow.setVisible(false);

        mJobWindow = new JobWindow(this, String.format("Progress [%s]", mOriginalPath));
        mJobWindow.setLocationRelativeTo(this);
    }

    private void loadParameterAndTarget(Model model) {
        mRelativePath = model.getOriginalModelPath();
        pnl_ParameterValue.loadParameterAndTarget(model);
    }

    private void progressStopActionPerformed(ActionEvent evt) {
    }

    private void progressCancelActionPerformed(ActionEvent evt) {
        if (cancelSimulation()) {
            ProgressPane progressPane = ProgressPane.getInstance();
            ProgressPane.ListCell cell
                    = progressPane.getListCellOfModel(new File(getRelativeModelPath()));

            int progress = cell.getStatusBarProgress();
            cell.progressFinished("finished", 0, 100, progress);
        }
    }

    public boolean cancelSimulation() {
        File modelFile = getModelFile();

        try {
            if (mSimulator == null || mSimulator.getSimulationDao() == null) {
                throw new IOException("It has not yet started");
            }

            TaskDao taskDao = mSimulator.getSimulationDao().obtainTask(modelFile);
            if (taskDao == null) {
                throw new IOException("It has not yet started");
            }

            int ans = JOptionPane.showConfirmDialog(this,
                    "Would you like to cancel simulation?",
                    "Cancel simulation?",
                    JOptionPane.YES_NO_OPTION);

            if (ans != JOptionPane.YES_OPTION) {
                return false;
            }

            taskDao.cancel();

            return true;
        } catch (IOException ex) {
            showErrorDialog("Cancellation failed\n\n" + ex.getMessage(),
                    "Cancellation failed");
            return false;
        }
    }

    private void progressLogActionPerformed(ActionEvent evt) {
        JButton logButton = (JButton) evt.getSource();
        mLogWindow.setVisible(true);
    }

    private void progressJobViewActionPerformed(ActionEvent evt) {
        JButton plotButton = (JButton) evt.getSource();
        mJobWindow.setVisible(true);
    }

    private void showMessageDialog(String message, String title) {
        JOptionPane.showMessageDialog(this, message, title,
                JOptionPane.DEFAULT_OPTION);
    }

    @Override
    public void showErrorDialog(ByteString message, String title) {
        JOptionPane.showMessageDialog(this, message.toStringUtf8(), title,
                JOptionPane.ERROR_MESSAGE);
    }

    private void setExportCsvAndPlotEnabled(boolean enabled) {
    }

    public void setStatusComponent (ProgressPane.ListCell pane) {

        if (mStatusComponent != null) {
            mStatusComponent.addPropertyChangeListener(mSelectionHandler);
        }
        mStatusComponent = pane;
        if (pane instanceof ProgressPane.ListCell) {
            ProgressPane.ListCell cell = ((ProgressPane.ListCell) pane);
            cell.addActionListener(this);
            cell.setGeneralButtonEnabled(false);
        }

        mStatusComponent.addPropertyChangeListener(mSelectionHandler);
    }

    public JComponent  getStatusComponent () {
        return mStatusComponent;
    }

    @Override
    public void onSimulationStarted(PhspSimulator.Event evt) {
        mSimulator = (PhspSimulator) evt.getSource();
        mJobWindow.setSimulator(mSimulator);
        mJobWindow.onSimulationStarted(evt);
        setEditable(false);
    }

    @Override
    public void onSimulationExited(PhspSimulator.Event evt) {
        mJobWindow.onSimulationExited(evt);
        setEditable(true);
    }

    public void reloadJobViewer() throws IOException, ParserConfigurationException {
        mJobWindow.setVisible(false);
        mJobWindow.dispose();
        mJobWindow = new JobWindow(this, String.format("Progress [%s]", mOriginalPath));
        mJobWindow.setLocationRelativeTo(this);


        TargetSet ts = getTargetSet();
        ParameterSet ps = getParameterSet();
        ParameterSet newPs = ps.filterByNames(Arrays.asList(ts.getUsingParameterNames()));
        mJobWindow.load(newPs);
    }

    public void setEditable(boolean editable) {
        mIsEditable = editable;

        pnl_GeneralSetting.setEnabled(editable);
        pnl_VariableSelection.setEnabled(editable);
        pnl_ParameterValue.setEnabled(editable);

        pnl_Content.setEnabledAt(0, editable);
        pnl_Content.setEnabledAt(1, editable);
        pnl_Content.setEnabledAt(2, editable);
    }

    public boolean isEditable() {
        return mIsEditable;
    }

    public IProgressManager getProgressManager() {
        return mJobWindow;
    }

    public void load(ISimulationConfiguration conf) {
        // focus on an input may prevent setting up its value, so clear focus
        KeyboardFocusManager focusManager = KeyboardFocusManager.getCurrentKeyboardFocusManager();
        focusManager.clearGlobalFocusOwner();

        // integration method
        pnl_GeneralSetting.setSelectedIntegrationMethodIndex(conf.getIntegrationMethod().getNumber());

        // length in the step unit
        pnl_GeneralSetting.setSimulationLegnth(conf.getLength());
        pnl_GeneralSetting.setSelectedStepUnit(mModelProbeResponse.getStepUnit().getName());

        // step in the step unit
        pnl_GeneralSetting.setSimulationStep(conf.getStep());
        pnl_GeneralSetting.setSelectedStepUnit(mModelProbeResponse.getStepUnit().getName());
        // granularity
        pnl_GeneralSetting.setGranularity(Integer.valueOf(conf.getGranularity()));

        // filter
        pnl_VariableSelection.setSelectedFilterSyntax(conf.getFilterSyntax());
        pnl_VariableSelection.setFilterPattern(conf.getFilterPattern());
        pnl_VariableSelection.setSelectedFilterColumn(conf.getFilterColumn());
    }

    public Ipc.ModelProbeResponse getModelProbeResponse() {
        return mModelProbeResponse;
    }

    public ParameterSet getParameterSet () {
        return pnl_ParameterValue.getParameterSet();
    }

    public TargetSet getTargetSet() {
        return pnl_ParameterValue.getTargetSet();
    }

    public void setPlotterSettingTabEnabled(boolean enabledFlg) {
    }

    public void cut() {
        mLogWindow.cut();
    }

    public void copy() {
        mLogWindow.copy();
    }

    @Override
    public void actionPerformed(ActionEvent evt) {
        String actionCommand = evt.getActionCommand();
        if (actionCommand == null)
            return;
        switch (actionCommand) {
        case ProgressPane.STOP_ACTION_COMMAND:
            progressStopActionPerformed(evt);
            break;
        case ProgressPane.CANCEL_ACTION_COMMAND:
            progressCancelActionPerformed(evt);
            break;
        case ProgressPane.LOG_ACTION_COMMAND:
            progressLogActionPerformed(evt);
            break;
        case ProgressPane.JOB_ACTION_COMMAND:
            progressJobViewActionPerformed(evt);
            break;
        }
    }

    public Model getModel () {
        Ipc.ModelProbeResponse response = mModelProbeResponse;
        Model.ModelFormat format = Model.ModelFormat.valueOf(response.getLanguage());

        Model model = new Model(format, mOriginalFile, mRelativePath);
        model.setParameterSet(getParameterSet());
        model.setTargetSet(getTargetSet());

        return model;
    }

    public File getModelFile() {
        return mOriginalFile;
    }

    @Override
    public String getModelCanonicalPath() {
        return mOriginalPath;
    }

    public void notifyK3Enabled() {
        if (mK3Loader.isEnabled()) {
        } else {
        }
    }

    @Override
    public void notifyModelFileModified() {
        ModelReloader reloader = new ModelReloader(mMainFrame, this);
        reloader.execute();
    }

    public boolean tryLock() {
        return mLock.tryLock();
    }

    public void unlock() {
        mLock.unlock();
    }

    public String getRelativeModelPath() {
        return mRelativePath;
    }

    @Override
    public Ipc.IntegrationMethod getIntegrationMethod() {
        return pnl_GeneralSetting.getIntegrationMethod();
    }

    @Override
    public String getLength() {
        return pnl_GeneralSetting.getLength();
    }

    @Override
    public String getStep() {
        return pnl_GeneralSetting.getStep();
    }

    @Override
    public int getGranularity() {
        return pnl_GeneralSetting.getGranularity();
    }

    @Override
    public ArrayList<String> getKeys() {
        return pnl_VariableSelection.getKeys();
    }

    @Override
    public int getFilterSyntax() {
        return pnl_VariableSelection.getSelectedFilterSyntaxIndex();
    }

    @Override
    public String getFilterPattern() {
        return pnl_VariableSelection.getFilterPattern();
    }

    @Override
    public int getFilterColumn() {
        return pnl_VariableSelection.getSelectedFilterColumnIndex();
    }

    @Override
    public void appendLog(String s) {
        Date date = new Date();
        SimpleDateFormat sdf = new SimpleDateFormat("yyyy-MM-dd HH:mm:ss");
        String d = sdf.format(date);
        mLogWindow.appendText("[" + d + "]" + s + System.getProperty("line.separator"));
    }

    @Override
    public void showErrorDialog(String message, String title) {
        JOptionPane.showMessageDialog(this, message, title,
                JOptionPane.ERROR_MESSAGE);
    }

    public static class SelectionHandler implements PropertyChangeListener {

        private final SubFrame mSubFrame;

        private SelectionHandler(SubFrame subFrame) {
            mSubFrame = subFrame;
        }

        @Override
        public void propertyChange(PropertyChangeEvent evt) {
            Component source = (Component)evt.getSource();
            String propertyName = evt.getPropertyName();
            if ("selected".equals(propertyName)) {
                boolean isSelected = (Boolean) evt.getNewValue();
                handleSelection(source, isSelected);
            }
        }

        boolean mSkip = false;

        private void handleSelection(Component src, boolean isSelected) {
            if (mSkip || !isSelected) return;

            if (src instanceof SubFrame) {
                SubFrame subFrame = (SubFrame)src;
                ProgressPane.ListCell cell = (ProgressPane.ListCell)subFrame.getStatusComponent();
                if (cell == null)
                    return;

                ProgressPane progressPane = ProgressPane.getInstance();

                mSkip = true;
                progressPane.setSelectedCell(cell, isSelected);
                mSkip = false;
            } else {
                ProgressPane.ListCell cell = (ProgressPane.ListCell)src;
                mSkip = true;
                try {
                    if (mSubFrame.isIcon())
                        mSubFrame.setIcon(false);
                    mSubFrame.setSelected(isSelected);
                } catch (PropertyVetoException ex) {
                    Logger.getLogger(SubFrame.class.getName()).log(Level.SEVERE, null, ex);
                }
                mSkip = false;
            }
        }

    }
}
