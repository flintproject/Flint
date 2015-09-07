/* -*- Mode: Java; tab-width: 4; indent-tabs-mode: nil; c-basic-offset: 4 -*- vim:set ts=4 sw=4 sts=4 et: */
package jp.oist.flint.desktop;

import jp.oist.flint.backend.ModelLoader;
import jp.oist.flint.dao.DaoException;
import jp.oist.flint.dao.SimulationDao;
import jp.oist.flint.dao.TaskDao;
import jp.oist.flint.executor.PhspProgressMonitor;
import jp.oist.flint.executor.PhspSimulator;
import jp.oist.flint.executor.SimulatorService;
import jp.oist.flint.filesystem.ModelFileWatcher;
import jp.oist.flint.form.MainFrame;
import jp.oist.flint.form.MessageDialog;
import jp.oist.flint.form.ModelFileLoaderListener;
import jp.oist.flint.form.ModelLoaderLogger;
import jp.oist.flint.form.ModelLoaderProgressDialog;
import jp.oist.flint.form.ProgressCell;
import jp.oist.flint.form.ProgressPane;
import jp.oist.flint.form.job.IProgressManager;
import jp.oist.flint.form.sub.SubFrame;
import jp.oist.flint.job.Progress;
import jp.oist.flint.phsp.IPhspConfiguration;
import jp.oist.flint.phsp.PhspException;
import jp.oist.flint.phsp.entity.Model;
import jp.oist.flint.sedml.SedmlException;
import org.apache.log4j.Logger;
import java.awt.HeadlessException;
import java.beans.PropertyChangeEvent;
import java.beans.PropertyChangeListener;
import java.beans.PropertyVetoException;
import java.io.BufferedReader;
import java.io.File;
import java.io.FileInputStream;
import java.io.IOException;
import java.io.InputStreamReader;
import java.nio.charset.StandardCharsets;
import java.sql.SQLException;
import java.util.ArrayList;
import java.util.Map;
import java.util.concurrent.ExecutionException;
import javax.swing.JDesktopPane;
import javax.swing.JInternalFrame;
import javax.swing.JOptionPane;
import javax.swing.SwingWorker;
import javax.swing.event.EventListenerList;
import javax.xml.parsers.ParserConfigurationException;
import javax.xml.transform.TransformerException;

/**
 * A desktop holds multiple documents; each of them corresponds to a model
 * opened by Flint.
 */
public class Desktop implements IPhspConfiguration {

    private final ArrayList<Document> mDocuments;

    private final EventListenerList mListeners;

    private final EventListenerList mLoadingListeners;

    private final EventListenerList mSimulationListeners;

    private final JDesktopPane mPane;

    private final ModelFileWatcher mModelFileWatcher;

    public Desktop() throws IOException {
        mDocuments = new ArrayList<>();
        mListeners = new EventListenerList();
        mLoadingListeners = new EventListenerList();
        mSimulationListeners = new EventListenerList();
        mPane = new JDesktopPane();
        mModelFileWatcher = new ModelFileWatcher();
    }

    public JDesktopPane getPane() {
        return mPane;
    }

    public int getSize() {
        return mDocuments.size();
    }

    public boolean isEmpty() {
        return mDocuments.isEmpty();
    }

    private void showErrorDialog(String message, String title) {
        JOptionPane.showMessageDialog(mPane, message, title, JOptionPane.ERROR_MESSAGE);
    }

    public boolean openFile(final File file) {
        if (file == null) {
            // just ignore it
            return false;
        }
        if (!file.exists()) {
            String msg = String.format("The file named \"%s\" does not exist.", file.getPath());
            showErrorDialog(msg, "Error on opening model");
            return false;
        }

        // check if the file is opened.
        for (Document doc : mDocuments) {
            if (file.equals(doc.getFile())) {
                try {
                    doc.getSubFrame().setSelected(true);
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

        ModelLoaderLogger logger = new ModelLoaderLogger(this);
        ModelLoader loader = new ModelLoader(file);
        loader.addPropertyChangeListener(new ModelFileLoaderListener(logger, loader));
        loader.addPropertyChangeListener(new ModelLoaderProgressDialog(null, path));
        for (ILoadingListener listener : mLoadingListeners.getListeners(ILoadingListener.class)) {
            loader.addPropertyChangeListener(new LoadingPropertyChangeListener(listener));
        }
        loader.execute();
        return true;
    }

    public void addDocument(Document document) throws IOException {
        SubFrame subFrame = new SubFrame(this, document);
        subFrame.setDefaultCloseOperation(JInternalFrame.DO_NOTHING_ON_CLOSE);
        subFrame.addInternalFrameListener(new SubFrameAdapter(this));

        try {
            mModelFileWatcher.watch(document.getFile(), subFrame);
        } catch (IOException ioe) {
            Logger.getRootLogger().error(ioe.getMessage());
        }

        subFrame.setVisible(true);
        mPane.add(subFrame);
        try {
            subFrame.setSelected(true);
            subFrame.setMaximum(true);
        } catch (PropertyVetoException ex) {
            // ignored
        }

        document.setSubFrame(subFrame);
        mDocuments.add(document);
        // notify listeners
        for (IDesktopListener listener : mListeners.getListeners(IDesktopListener.class)) {
            listener.documentAdded(document);
        }
    }

    public void removeDocument(Document document) {
        mModelFileWatcher.unwatch(document.getFile());
        SubFrame subFrame = document.getSubFrame();
        subFrame.dispose();
        mDocuments.remove(document);
        // notify listeners
        for (IDesktopListener listener : mListeners.getListeners(IDesktopListener.class)) {
            listener.documentRemoved(document, isEmpty());
        }
    }

    public PhspSimulator runSimulation(final MainFrame mainFrame)
        throws IOException, ParserConfigurationException, PhspException,
               SQLException, SedmlException, TransformerException {
        for (SubFrame subFrame : getSubFrames())
            subFrame.reloadJobViewer();

        SimulatorService service = new SimulatorService(mainFrame);
        final PhspSimulator simulator = new PhspSimulator(service, mainFrame, this);
        final PhspProgressMonitor monitor = new PhspProgressMonitor(simulator);
        simulator.addPropertyChangeListener(new PropertyChangeListener() {
            @Override
            public void propertyChange(PropertyChangeEvent e) {
                String propertyName = e.getPropertyName();
                Object nv = e.getNewValue();
                if ("state".equals(propertyName)) {
                    if (SwingWorker.StateValue.DONE.equals(nv)) {
                        try {
                            monitor.stop();
                            //TODO
                            Boolean result = simulator.get();
                            if (result) {
                                JOptionPane.showMessageDialog(mainFrame,
                                                              "Simulation completed", "Simulation completed",
                                                              JOptionPane.PLAIN_MESSAGE);
                            }
                        } catch (InterruptedException | ExecutionException | HeadlessException ex) {
                            File logFile = simulator.getLogFile();
                            StringBuilder sb = new StringBuilder();
                            if (logFile != null) {
                                try (FileInputStream fis = new FileInputStream(logFile);
                                     InputStreamReader isr = new InputStreamReader(fis, StandardCharsets.UTF_8);
                                     BufferedReader reader = new BufferedReader(isr)) {
                                    String line;
                                    while ((line = reader.readLine()) != null) {
                                        sb.append(line).append(System.getProperty("line.separator"));
                                    }
                                } catch (IOException ex1) {
                                    Logger.getRootLogger().error(ex1.getMessage());
                                }
                            }
                            String detail = sb.toString();
                            MessageDialog.showMessageDialog(mainFrame,
                                                            "The following error occurred during simulation:",
                                                            detail,
                                                            "Error on simulation",
                                                            JOptionPane.ERROR_MESSAGE, null, new Object[]{"OK"});
                        }
                    }
                }
            }
        });
        for (SubFrame subFrame : getSubFrames())
            simulator.addPropertyChangeListener(new SimulationPropertyChangeListener(subFrame));
        for (ISimulationListener listener : mSimulationListeners.getListeners(ISimulationListener.class)) {
            simulator.addPropertyChangeListener(new SimulationPropertyChangeListener(listener));
        }

        monitor.addPropertyChangeListener(new PropertyChangeListener() {
            @Override
            public void propertyChange(PropertyChangeEvent e) {
                String propertyName = e.getPropertyName();
                if ("progress".equals(propertyName)) {
                    if (e instanceof PhspProgressMonitor.Event) {
                        final PhspProgressMonitor.Event evt = (PhspProgressMonitor.Event)e;
                        String modelPath = (String)evt.getClientProperty("modelPath");
                        SubFrame subFrame = mainFrame.findSubFrame(modelPath);

                        SimulationDao simulationDao = simulator.getSimulationDao();
                        try {
                            TaskDao taskDao = simulationDao.obtainTask(new File(subFrame.getRelativeModelPath()));

                            Progress progress = (Progress)evt.getNewValue();
                            Map<String, Number> target = (Map<String, Number>)evt.getClientProperty("target");
                            IProgressManager progressMgr = subFrame.getProgressManager();
                            int index = progressMgr.indexOf(target);

                            progressMgr.setProgress(index, progress);

                            if (taskDao.isCancelled())
                                progressMgr.setCancelled(index, true);

                            int taskProgress = taskDao.getProgress();
                            ProgressCell cell =
                                ProgressPane.getInstance().getListCellOfModel(new File(modelPath));

                            String status;
                            if (taskDao.isFinished()) {
                                status = (taskDao.isCancelled())? "finished" : "completed";
                                cell.progressFinished(status, 0, 100, taskProgress);
                            } else if (taskDao.isStarted()) {
                                status = (taskDao.isCancelled())? "cancelling..." : taskProgress + " %";
                                cell.setProgress(status, 0, 100, taskProgress);
                            }
                        } catch (DaoException | IOException | SQLException ex) {
                            // ignore the error
                        }
                    }
                }
            }
        });

        simulator.execute();
        monitor.start();
        return simulator;
    }

    public void addListener(IDesktopListener listener) {
        mListeners.add(IDesktopListener.class, listener);
    }

    public void addLoadingListener(ILoadingListener listener) {
        mLoadingListeners.add(ILoadingListener.class, listener);
    }

    public void addSimulationListener(ISimulationListener listener) {
        mSimulationListeners.add(ISimulationListener.class, listener);
    }

    public void startWatching() {
        Thread t = new Thread(mModelFileWatcher);
        t.start();
    }

    public ArrayList<SubFrame> getSubFrames() {
        ArrayList<SubFrame> list = new ArrayList<>();
        for (Document doc : mDocuments) {
            list.add(doc.getSubFrame());
        }
        return list;
    }

    /* IPhspConfiguration */

    @Override
    public Model[] getModels() throws PhspException {
        int size = mDocuments.size();
        Model[] models = new Model[size];
        for (int i=0;i<size;i++) {
            models[i] = mDocuments.get(i).getSubFrame().getModel();
        }
        return models;
    }
}
