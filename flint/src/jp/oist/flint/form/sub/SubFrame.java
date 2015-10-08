/* -*- Mode: Java; tab-width: 4; indent-tabs-mode: nil; c-basic-offset: 4 -*- vim:set ts=4 sw=4 sts=4 et: */
package jp.oist.flint.form.sub;

import jp.oist.flint.executor.ModelReloader;
import jp.oist.flint.dao.DaoException;
import jp.oist.flint.desktop.Desktop;
import jp.oist.flint.desktop.Document;
import jp.oist.flint.desktop.ISimulationListener;
import jp.oist.flint.form.ProgressCellSelectionListener;
import jp.oist.flint.phsp.PhspException;
import jp.oist.flint.util.IntegrationMethodFormat;
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
import java.util.concurrent.ExecutionException;
import java.util.concurrent.locks.Lock;
import java.util.concurrent.locks.ReentrantLock;
import javax.swing.JFrame;
import javax.swing.JOptionPane;
import javax.swing.JTabbedPane;
import javax.swing.JInternalFrame;
import com.google.protobuf.ByteString;
import java.sql.SQLException;
import java.util.Arrays;
import javax.swing.JComponent;
import javax.xml.parsers.ParserConfigurationException;
import jp.oist.flint.dao.TaskDao;
import jp.oist.flint.backend.ModelLoader;
import jp.oist.flint.executor.PhspSimulator;
import jp.oist.flint.filesystem.IModelFileClient;
import jp.oist.flint.form.IFrame;
import jp.oist.flint.form.ProgressCell;
import jp.oist.flint.form.job.IProgressManager;
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
               ISimulationConfiguration, ISimulationListener, IFrame {

    public final static String ID = "flint.view.sub";

    private final Desktop mDesktop;
    private final Document mDocument;
    private final File mOriginalFile;
    private final String mOriginalPath;
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

    public SubFrame(Desktop desktop, Model model, ModelLoader loader)
            throws IOException, ExecutionException, InterruptedException {
        this(desktop, loader.get());

        loadParameterAndTarget(model);
    }

    static int mAutoIncrement = 0;

    public SubFrame(Desktop desktop, Document document) throws IOException {
        super(document.getFile().getAbsolutePath(), true, true, true, true);

        mDesktop = desktop;
        mDocument = document;
        File file = document.getFile();
        mOriginalFile = file;
        mOriginalPath = file.getCanonicalPath();
        mRelativePath = file.getPath();
        mK3Loader = new K3Loader();

        init(document);
    }

    private void init(Document document) throws IOException {
        Ipc.ModelProbeResponse response = document.getResponse();

        initComponents();

        initSubWindow();

        String integrationMethod = IntegrationMethodFormat.name(response.getIntegrationMethod());
        if (integrationMethod != null)
            pnl_GeneralSetting.setSelectedIntegrationMethod(integrationMethod);
        pnl_GeneralSetting.setSimulationLegnth(response.getLength());
        pnl_GeneralSetting.setSimulationStep(response.getStep());

        int timeUnitCount = response.getTimeUnitCount();
        ArrayList<String> options = new ArrayList<>(timeUnitCount);

        for (int i = 0; i < timeUnitCount; i++) {
            options.add(response.getTimeUnit(i).getName());
        }

        pnl_GeneralSetting.initializeUnitItems(options);
        pnl_GeneralSetting.setSelectedLengthUnit(response.getLengthUnit().getName());
        pnl_GeneralSetting.setSelectedStepUnit(response.getStepUnit().getName());
        pnl_GeneralSetting.setSelectedOutputStartTimeUnit(response.getLengthUnit().getName());

        pnl_VariableSelection.setSelectedFilterSyntax(1);

        setVisible(true);
    }

    private void initComponents() throws IOException {
        Dimension dim = new Dimension(600, 400);

        setMaximumSize(new Dimension(Short.MAX_VALUE, Short.MAX_VALUE));
        setMinimumSize(dim);
        setPreferredSize(dim);
        setSize(dim);

        pnl_Content = new JTabbedPane();
        pnl_Content.setOpaque(true);
        setContentPane(pnl_Content);

        pnl_GeneralSetting = new GeneralSettingPane(mDocument.getResponse());
        pnl_Content.addTab("General Settings", pnl_GeneralSetting);

        pnl_VariableSelection = new VariableSelectionPane(mDocument.getResponse());
        pnl_Content.addTab("Output Variables", pnl_VariableSelection);

        pnl_ParameterValue = new ParameterValuePane(mDocument);
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

    public Document getDocument() {
        return mDocument;
    }

    public boolean cancelSimulation() {
        File modelFile = getModelFile();

        try {
            if (mSimulator == null || mSimulator.getSimulationDao() == null) {
                throw new IOException("It has not yet started");
            }

            TaskDao taskDao = mSimulator.getSimulationDao().obtainTask(modelFile.getPath());
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
        } catch (DaoException | IOException | SQLException ex) {
            showErrorDialog("Cancellation failed\n\n" + ex.getMessage(),
                    "Cancellation failed");
            return false;
        }
    }

    @Override
    public void showErrorDialog(ByteString message, String title) {
        JOptionPane.showMessageDialog(this, message.toStringUtf8(), title,
                JOptionPane.ERROR_MESSAGE);
    }

    public void setStatusComponent(ProgressCell pane) {
        assert pane != null;
        pane.addActionListener(this);
        pane.setGeneralButtonEnabled(false);
        mStatusComponent = pane;
        mStatusComponent.addPropertyChangeListener(new ProgressCellSelectionListener(this));
    }

    public JComponent  getStatusComponent () {
        return mStatusComponent;
    }

    /* ISimulationListener */

    @Override
    public void simulationStarted(PhspSimulator simulator) {
        mSimulator = simulator;
        mJobWindow.setSimulator(mSimulator);
        mJobWindow.simulationStarted(simulator);
        setEditable(false);
    }

    @Override
    public void simulationDone() {
        mJobWindow.simulationDone();
        setEditable(true);
    }

    @Override
    public void simulationPaused() {
    }

    @Override
    public void simulationResumed() {
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
        pnl_GeneralSetting.setSelectedStepUnit(mDocument.getResponse().getStepUnit().getName());

        // step in the step unit
        pnl_GeneralSetting.setSimulationStep(conf.getStep());
        pnl_GeneralSetting.setSelectedStepUnit(mDocument.getResponse().getStepUnit().getName());
        // granularity
        pnl_GeneralSetting.setGranularity(Integer.valueOf(conf.getGranularity()));

        // filter
        pnl_VariableSelection.setSelectedFilterSyntax(conf.getFilterSyntax());
        pnl_VariableSelection.setFilterPattern(conf.getFilterPattern());
        pnl_VariableSelection.setSelectedFilterColumn(conf.getFilterColumn());
    }

    public Ipc.ModelProbeResponse getModelProbeResponse() {
        return mDocument.getResponse();
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
        mJobWindow.setVisible(true);
    }

    public Model getModel() throws PhspException {
        Ipc.ModelProbeResponse response = mDocument.getResponse();
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
        ModelReloader reloader = new ModelReloader(mDesktop, this);
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
    public String getOutputStartTime() {
        return pnl_GeneralSetting.getOutputStartTime();
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

}
