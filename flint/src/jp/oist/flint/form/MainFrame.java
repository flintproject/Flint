/* -*- Mode: Java; tab-width: 4; indent-tabs-mode: nil; c-basic-offset: 4 -*- vim:set ts=4 sw=4 sts=4 et: */
package jp.oist.flint.form;

import jp.oist.flint.backend.ModelLoader;
import jp.oist.flint.control.FileChooser;
import jp.oist.flint.control.ModelFileTransferHandler;
import jp.oist.flint.desktop.Desktop;
import jp.oist.flint.desktop.Document;
import jp.oist.flint.desktop.IDesktopListener;
import jp.oist.flint.executor.PhspSimulator;
import jp.oist.flint.form.sub.SubFrame;
import jp.oist.flint.k3.K3Client;
import jp.oist.flint.k3.K3Request;
import jp.oist.flint.k3.K3RequestBuilder;
import jp.oist.flint.phsp.PhspException;
import jp.oist.flint.phsp.PhspReader;
import jp.oist.flint.phsp.PhspReaderListener;
import jp.oist.flint.phsp.PhspWriter;
import jp.oist.flint.rpc.ICallee;
import jp.oist.flint.sedml.ISimulationConfiguration;
import jp.oist.flint.sedml.ISimulationConfigurationList;
import jp.oist.flint.sedml.SedmlException;
import jp.oist.flint.sedml.SedmlReader;
import jp.oist.flint.sedml.SedmlWriter;
import jp.oist.flint.textformula.analyzer.ParseException;
import jp.oist.flint.theme.Icon;
import jp.oist.flint.util.Utility;
import org.apache.log4j.Logger;
import org.xml.sax.SAXException;
import java.awt.BorderLayout;
import java.awt.Dimension;
import java.awt.HeadlessException;
import java.beans.PropertyChangeEvent;
import java.beans.PropertyChangeListener;
import java.beans.PropertyVetoException;
import java.io.File;
import java.io.FileOutputStream;
import java.io.IOException;
import java.nio.file.Path;
import java.nio.file.Paths;
import java.sql.SQLException;
import java.text.SimpleDateFormat;
import java.util.Date;
import java.util.List;
import java.util.concurrent.ExecutionException;
import java.util.prefs.BackingStoreException;
import java.util.prefs.Preferences;
import javax.swing.JComponent;
import javax.swing.JFileChooser;
import javax.swing.JOptionPane;
import javax.swing.JPanel;
import javax.swing.JSplitPane;
import javax.swing.SwingUtilities;
import javax.swing.SwingWorker;
import javax.swing.WindowConstants;
import javax.swing.filechooser.FileNameExtensionFilter;
import javax.xml.parsers.ParserConfigurationException;
import javax.xml.transform.TransformerException;

/**
 * This is the class of the main window.
 */
public class MainFrame extends javax.swing.JFrame 
    implements ICallee, IDesktopListener,
               ISimulationConfigurationList, IFrame {

    static {
        try {
            jp.oist.flint.plotter.PlotterLoader.register("gnuplot", jp.oist.flint.gnuplot.Plotter.class);
        } catch (BackingStoreException bse) {
            Logger.getRootLogger().error(bse.getMessage());
        }
    }

    public final static int MIN_WIDTH = 800;

    public final static int MIN_HEIGHT = 600;

    private final Desktop mDesktop;

    private File mPhspFile = null;

    private PhspSimulator mSimulator = null;

    public MainFrame(Desktop desktop, ControlPane controlPane, ProgressPane progressPane)
        throws IOException {
        super();
        mDesktop = desktop;
        setIconImage(Icon.getImage());
        setTransferHandler(new ModelFileTransferHandler(this));

        initComponents(controlPane, progressPane);
    }

    private void initComponents(ControlPane controlPane, ProgressPane progressPane) {
        setDefaultCloseOperation(WindowConstants.EXIT_ON_CLOSE);
        setTitle("Flint");

        setMinimumSize(new Dimension(MIN_WIDTH, MIN_HEIGHT));
        setMaximumSize(new Dimension(
            Short.MAX_VALUE, Short.MAX_VALUE
        ));
        setSize(new Dimension(MIN_WIDTH, MIN_HEIGHT));
        setPreferredSize(new Dimension(MIN_WIDTH, MIN_HEIGHT));
        setLocationRelativeTo(null);

        setContentPane(createContentPane(controlPane, progressPane));
        pack();
    }

    private JComponent createContentPane(ControlPane controlPane, ProgressPane progressPane) {
        final JSplitPane contentPane = new JSplitPane(JSplitPane.HORIZONTAL_SPLIT);
        contentPane.setDividerSize(10);
        contentPane.setDividerLocation(638);
        contentPane.setOneTouchExpandable(true);

        final JPanel peripheralPane = new JPanel(new BorderLayout());
        peripheralPane.setEnabled(false);

        peripheralPane.setMaximumSize(new Dimension(Short.MAX_VALUE, Short.MAX_VALUE));
        peripheralPane.setMinimumSize(new Dimension(150, Short.MAX_VALUE));

        contentPane.setLeftComponent(mDesktop.getPane());
        contentPane.setRightComponent(peripheralPane);

        progressPane.setMaximumSize(new Dimension(Short.MAX_VALUE, Short.MAX_VALUE));
        progressPane.setMinimumSize(new Dimension(0, 0));
        progressPane.setPreferredSize(new Dimension(150, 510));

        controlPane.setMaximumSize(new Dimension(Short.MAX_VALUE, 60));
        controlPane.setMinimumSize(new Dimension(0, 60));
        controlPane.setPreferredSize(new Dimension(150, 60));

        peripheralPane.add(progressPane, BorderLayout.CENTER);
        peripheralPane.add(controlPane, BorderLayout.SOUTH);

        return contentPane;
    }

    public boolean openPhsp(final File phspFile) {
        List<SubFrame> subFrames = getSubFrames();
        int numberOfSubView = subFrames.size();
        if (numberOfSubView > 0) {
            int ans = JOptionPane.showConfirmDialog(this,
                            "Is it OK to close existing windows?",
                            "Close windows",
                            JOptionPane.YES_NO_OPTION);

            if (ans != JOptionPane.YES_OPTION) return false;
            closeAll();
        }

        mPhspFile = phspFile;
            setEditable(false);
            ModelLoaderLogger logger = new ModelLoaderLogger(mDesktop);
            PhspReader phspLoader = new PhspReader(phspFile);
            phspLoader.addPropertyChangeListener(new PhspReaderListener(logger));
            phspLoader.addPropertyChangeListener(new PropertyChangeListener() {
                @Override
                public void propertyChange(PropertyChangeEvent evt) {
                    String propertyName = evt.getPropertyName();
                    Object newValue = evt.getNewValue();
                    if ("state".equals(propertyName) 
                            && SwingWorker.StateValue.DONE.equals(newValue)) {
                        setEditable(true);
                    }
                }
            });
            phspLoader.execute();
            return true;
    }

    public boolean closeModel (SubFrame subFrame) {
        if (mSimulator != null && mSimulator.isStarted()) {
            JOptionPane.showMessageDialog(this, 
                                          "Could not close the model due to running simulation.",
                                          "Error on closing model",
                    JOptionPane.INFORMATION_MESSAGE);
            return false;
        }

        if (subFrame == null) 
            return false;

        if (!subFrame.isClosed())
            mDesktop.removeDocument(subFrame.getDocument());
        return true;
    }

    public void closeAll () {
        for (SubFrame subFrame : getSubFrames())
            closeModel(subFrame);
    }

    public SubFrame getSelectedSubFrame () {
        return (SubFrame) mDesktop.getPane().getSelectedFrame();
    }

    public List<SubFrame> getSubFrames() {
        return mDesktop.getSubFrames();
    }

    public void setEditable (boolean editable) {
        List<SubFrame> subFrames = getSubFrames();
        for (SubFrame subFrame : subFrames)
            subFrame.setEditable(editable);
    }

    public void openPerformed(String lastPath) {
        FileChooser fc = new FileChooser(this, "Open model", FileChooser.Mode.LOAD, lastPath);
        JFileChooser jfc = fc.getJFileChooser();
        if (jfc != null) {
            jfc.setAcceptAllFileFilterUsed(false);
            
            FileNameExtensionFilter xmlFilter = new FileNameExtensionFilter("XML files (*.xml)", "xml"); 
            FileNameExtensionFilter modelFilter = new FileNameExtensionFilter("Model files (*.isml, *.phml, *.phz, *.sbml)", "isml", "phml", "phz", "sbml");
            FileNameExtensionFilter phspFilter = new FileNameExtensionFilter("PHSP files (*.phsp)", "phsp");
            jfc.addChoosableFileFilter(modelFilter);
            jfc.addChoosableFileFilter(phspFilter);
            jfc.addChoosableFileFilter(xmlFilter);
            jfc.addChoosableFileFilter(jfc.getAcceptAllFileFilter());
            jfc.setFileFilter(modelFilter);
        }

        if (!fc.showDialog()) return;

        openModel(fc.getSelectedFile());
    }

    public void recentModelPerformed (Object source, File f) {
        openModel(f);
    }

    public void closePerformed (Object source) {
        closeModel(getSelectedSubFrame());
    }

    public void loadConfigurationPerformed (Object source) {
        Preferences prefs = Preferences.userRoot().node("/jp/oist/flint");
        String defaultPath = prefs.get("defaultConfigurationPath", "");

        FileChooser fc = new FileChooser(this, "Open SED-ML file", FileChooser.Mode.LOAD, defaultPath);
        JFileChooser jfc = fc.getJFileChooser();
        if (jfc != null) {
            jfc.setAcceptAllFileFilterUsed(false);

            FileNameExtensionFilter sedmlFilter = new FileNameExtensionFilter("SED-ML files (*.sedml, *.xml)", "sedml", "xml");

            jfc.addChoosableFileFilter(sedmlFilter);
            jfc.addChoosableFileFilter(jfc.getAcceptAllFileFilter());
            jfc.setFileFilter(sedmlFilter);
        }

        if (!fc.showDialog()) return;
        File file = fc.getSelectedFile();
        SedmlReader reader = new SedmlReader(file);
        try {
            if (reader.parse()) {
                prefs.put("defaultConfigurationPath", file.getParent());
                ISimulationConfigurationList configs = (ISimulationConfigurationList)reader.getHandler();

                for (SubFrame subFrame : getSubFrames()) {
                    ISimulationConfiguration config = 
                            configs.getConfigurationByModelPath(subFrame.getRelativeModelPath());
                    subFrame.load(config);
                }
            }
        } catch (IOException | ParserConfigurationException | SAXException e) {
            showErrorDialog(e.getMessage(), "Error on loading configuration");
        }
    }

    public void saveConfigurationPerformed (Object source) {
        FileChooser fc = new FileChooser(this, "Select SED-ML file", FileChooser.Mode.SAVE, "");
        if (fc.showDialog()) {
            final File file = fc.getSelectedFile();
            if (file.exists()) {
                int ans = JOptionPane.showConfirmDialog(this,
                                                        "Is it OK to replace the existing file?",
                                                        "Replace the existing file?",
                                                        JOptionPane.YES_NO_OPTION);
                if (ans != JOptionPane.YES_OPTION) return;
            }
            try (FileOutputStream fos = new FileOutputStream(file)) {
                SedmlWriter writer = new SedmlWriter(true);
                writer.writeSimulationConfiguration(this, fos);
            } catch (HeadlessException e) {
                showErrorDialog(e.getMessage(), "Error on saving configuration");
                return;
            } catch (IOException | ArithmeticException | SedmlException e) {
                showErrorDialog(e.getMessage(), "Error on saving configuration");
                return;
            }
            JOptionPane.showMessageDialog(this, "Saved configuration as " + file.getPath());
        }
    }

    public void saveAsPhspPerformed (Object source) {
        try {
            if (mDesktop.isEmpty()) {
                showErrorDialog("Please open some models.",
                                "Error on saving PHSP");
                return;
            }

            FileChooser fc = new FileChooser(this, "Select PHSP file", FileChooser.Mode.SAVE, "");
            JFileChooser jfc = fc.getJFileChooser();
            FileNameExtensionFilter xmlFilter = new FileNameExtensionFilter("XML files (*.xml)", "xml");
            FileNameExtensionFilter modelFilter = new FileNameExtensionFilter("PHSP files (*.phsp)", "phsp");
            jfc.addChoosableFileFilter(modelFilter);
            jfc.addChoosableFileFilter(xmlFilter);
            jfc.addChoosableFileFilter(jfc.getAcceptAllFileFilter());
            jfc.setFileFilter(modelFilter);
            File initFile = (mPhspFile == null)? new File("Untitled.phsp"): mPhspFile;
            jfc.setSelectedFile(initFile);

           if (fc.showDialog()) {
                File file = jfc.getSelectedFile();
                if (!file.getName().endsWith(".phsp"))
                    file = new File(file.getPath()+".phsp");

                if (file.exists()) {
                    int ans = JOptionPane.showConfirmDialog(this,
                                                            "Is it OK to replace the existing file?",
                                                            "Replace the existing file?",
                                                            JOptionPane.YES_NO_OPTION);
                    if (ans != JOptionPane.YES_OPTION) return;
                }

                setEnabled(false);

                try (FileOutputStream fos = new FileOutputStream(file)) {
                PhspWriter writer = new PhspWriter();
                writer.write(mDesktop, fos, false);
                }
                JOptionPane.showMessageDialog(this, "Saved phsp as " + file.getPath());
           }
        } catch (IOException |
                 ParseException |
                 ParserConfigurationException |
                 PhspException |
                 TransformerException ex) {
            showErrorDialog(ex.getMessage(), "Saving as PHSP failed");
        } finally {
            setEnabled(true);
        }
    }

    public void exitPerformed (Object source) {
        if (mSimulator != null)
            mSimulator.cancel(true);
        System.exit(0);
    }

    public void copyPerformed (Object source) {
        SubFrame subFrame = getSelectedSubFrame();
        subFrame.copy();
    }

    public void cutPerformed (Object source) {
        SubFrame subFrame = getSelectedSubFrame();
        subFrame.cut();
    }

    public void preferencePerformed (Object source) {
       PreferenceDialog ad = new PreferenceDialog(this, true);
        ad.setVisible(true);
    }

    public void simulationRunPerformed(Object source) {
        try {
            mSimulator = mDesktop.runSimulation(this);
        } catch (IOException |
                 ParseException |
                 ParserConfigurationException |
                 PhspException |
                 SQLException |
                 SedmlException |
                 TransformerException ex) {
            showErrorDialog(ex.getMessage(), "ERROR");
        }
    }

    public void simulationPausePerformed(Object source) {
        if (mSimulator != null) {
            mDesktop.pauseSimulation(new File(mSimulator.getWorkingDirectory(), "pid.txt"));
        }
    }

    public void simulationResumePerformed(Object source) {
        if (mSimulator != null) {
            mDesktop.resumeSimulation(new File(mSimulator.getWorkingDirectory(), "pid.txt"));
        }
    }

    public void sendToK3Performed(Object source) {
        Preferences prefs = Preferences.userRoot().node("/jp/oist/flint/session/k3");
        final String encryptedUserId = prefs.get("encryptedUserId", null); 
        final String encryptedPassword = prefs.get("encryptedPassword", null); 

        if (encryptedUserId == null  || encryptedUserId.isEmpty() 
                || encryptedPassword == null || encryptedPassword.isEmpty()) {
            StringBuilder sb = new StringBuilder("Please specify your account of Flint K3.");
            sb.append(System.getProperty("line.separator"));
            sb.append("(Edit -> Preference -> K3)");
            showErrorDialog(sb.toString(), "Error on preference");
            return;
        }

        SubFrame subFrame  = getSelectedSubFrame();
        final Object retval = JOptionPane.showInputDialog(this,
                                                          "New job's title:",
                                                          "New job's title",
                 JOptionPane.QUESTION_MESSAGE, null, null,
                 subFrame.getModelFile().getName());

        if (retval == null)
            return;

        SwingWorker<Integer, Void> worker = new SwingWorker<Integer, Void>() {
            @Override
            protected Integer doInBackground() throws Exception {
                SubFrame subFrame  = getSelectedSubFrame();
                String jobName = (String)retval;
                String userId = Utility.decrypt(encryptedUserId);
                String passwd = Utility.decrypt(encryptedPassword);

                K3RequestBuilder reqBuilder = new K3RequestBuilder(
                        subFrame.getModelFile(), subFrame);
                K3Request request = reqBuilder.build(jobName, userId, passwd);

                K3Client k3 = new K3Client();
                return k3.submit(request);
            }
            @Override
            protected void done () {
                try {
                    int jobId = get();
                    String message = String.format("Submitted successfully your job to Flint K3 (Job ID : %d)", jobId);
                    String title = "Job submitted to Flint K3";
                    JOptionPane.showMessageDialog(MainFrame.this,
                                                  message,
                                                  title,
                                                  JOptionPane.INFORMATION_MESSAGE);
                } catch (InterruptedException | ExecutionException ex) {
                    showErrorDialog(ex.getMessage(),
                                    "Error on communicating with Flint K3");
                }
            }
        };
        worker.execute();
    }

    public void aboutPerformed (Object source) {
        AboutDialog ad = new AboutDialog(this);
        ad.setVisible(true);
    }

    /*
     * Implements IFrame 
     */
    @Override
    public void showErrorDialog(String message, String title) {
        JOptionPane.showMessageDialog(this, message, title, JOptionPane.ERROR_MESSAGE);
    }

    @Override
    public void appendLog(String s) {
        Date date = new Date();
        SimpleDateFormat sdf = new SimpleDateFormat("yyyy-MM-dd HH:mm:ss");
        String d = sdf.format(date);

        Logger.getRootLogger().error("[" + d + "] " +  s  + System.getProperty("line.separator"));
    }

    public void notifyK3Enabled() {
        List<SubFrame> subFrames = getSubFrames();
        for (SubFrame subFrame : subFrames) {
            subFrame.notifyK3Enabled();
        }
    }

    public boolean openModel (final File file) {
        if (file == null) {
            // just ignore it
            return false;
        }
        if (!file.exists()) {
            String separator = System.getProperty("line.separator");
            String msg = String.format("The file named \"%s\" " + separator
                    + "does not exist.", file.getPath());
            showErrorDialog(msg, "Error on opening model");
            return false;
        }

        // check xml format
        String format = Utility.detectXMLFormat(file);

        if ("phsp".equals(format)) 
            return openPhsp(file);

        // check if the file is opened.
        for (SubFrame child : getSubFrames()) {
           if (child.getModelFile().getPath().equals(file.getPath())) {
               try {
                   child.setSelected(true);
               } catch (PropertyVetoException ex) {
                    // ignored
               }
               return true;
            }
        }

        String path;
        try {
            path = file.getCanonicalPath();
        } catch (IOException ex) {
            showErrorDialog("could not get canonical path : " + file.toString(),
                    "Error on opening model");
            return false;
        }

        if (!file.isFile()) {
            showErrorDialog("could not get canonical path : " + file.toString(),
                    "Error on opening model"); return false; }

        int len = (int)file.length();
        if (len == 0) {
            showErrorDialog("file has length 0 : " + path,
                    "Error on opening model");
            return false;
        }

        ModelLoaderLogger logger = new ModelLoaderLogger(mDesktop);
        setEditable(false);
        ModelLoader loader = new ModelLoader(file);
        loader.addPropertyChangeListener(new ModelFileLoaderListener(logger, loader));
        loader.addPropertyChangeListener(new ModelLoaderProgressDialog(this, path));
        loader.addPropertyChangeListener(new PropertyChangeListener() {
            @Override
            public void propertyChange(PropertyChangeEvent evt) {
                String propertyName = evt.getPropertyName();
                Object newValue = evt.getNewValue();
                if ("state".equals(propertyName) 
                    && SwingWorker.StateValue.DONE.equals(newValue)) {
                    setEditable(true);
                }
            }
        });
        loader.execute();
        return true;
    }

    @Override
    public void openModel(final String name) {
        SwingUtilities.invokeLater(new Runnable() {
            @Override
            public void run() {
                Path path = Paths.get(name);
                openModel(path.toFile());
            }
        });
    }

    public void setPlotterSettingTabEnabled(String defaultPlotter) {
        SubFrame subFrame = this.getSelectedSubFrame();
        if (subFrame != null) {
            subFrame.setPlotterSettingTabEnabled("gnuplot".equals(defaultPlotter));
        }
    }

    /*
     * implements ISimulationConfigurationList 
     */
    @Override
    public ISimulationConfiguration getConfiguration(int index) {
        return getSubFrames().get(index);
    }

    @Override
    public ISimulationConfiguration getConfigurationByModelPath(String modelPath) {
        return findSubFrame(modelPath);
    }

    public SubFrame findSubFrame (String modelPath) {
        for (SubFrame subFrame : getSubFrames()) {
            if (modelPath.equals(subFrame.getRelativeModelPath()))
                return subFrame;
        }
        return null;
    }

    @Override
    public int getConfigurationCount () {
        return mDesktop.getSize();
    }

    /* IDesktopListener */

    @Override
    public void documentAdded(Document doc) {
        Preferences prefs = Preferences.userRoot().node("/jp/oist/flint");
        String defaultPlotter = prefs.get("defaultPlotter", "");
        setPlotterSettingTabEnabled(defaultPlotter);
    }

    @Override
    public void documentRemoved(Document doc, boolean empty) {
        if (empty)
            requestFocus();
    }
}
