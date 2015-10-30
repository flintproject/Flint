/* -*- Mode: Java; tab-width: 4; indent-tabs-mode: nil; c-basic-offset: 4 -*- vim:set ts=4 sw=4 sts=4 et: */
package jp.oist.flint.form;

import jp.oist.flint.control.FileChooser;
import jp.oist.flint.dao.DaoException;
import jp.oist.flint.dao.SimulationDao;
import jp.oist.flint.dao.TaskDao;
import jp.oist.flint.export.ExportReceiver;
import jp.oist.flint.export.ExportWorker;
import jp.oist.flint.filesystem.Workspace;
import jp.oist.flint.form.job.GadgetDialog;
import jp.oist.flint.form.job.PlotWindow;
import jp.oist.flint.form.sub.InputDialogForExport;
import jp.oist.flint.garuda.GarudaClient;
import jp.oist.flint.job.Job;
import jp.oist.flint.util.Utility;
import jp.physiome.Ipc;
import jp.sbi.garuda.platform.commons.net.GarudaConnectionNotInitializedException;
import com.google.protobuf.ByteString;
import java.beans.PropertyChangeEvent;
import java.beans.PropertyChangeListener;
import java.io.File;
import java.io.FileInputStream;
import java.io.IOException;
import java.sql.SQLException;
import java.util.concurrent.CancellationException;
import java.util.concurrent.ExecutionException;
import javax.swing.JFrame;
import javax.swing.JOptionPane;
import javax.swing.SwingWorker;

public class JobMenu implements IFrame {

    private final JFrame mFrame;

    private final SimulationDao mSimulationDao;

    private final int mTaskId;

    private final int mJobId;

    public JobMenu(JFrame frame, SimulationDao simulationDao, int taskId, int jobId) {
        mFrame = frame;
        mSimulationDao = simulationDao;
        mTaskId = taskId;
        mJobId = jobId;
    }

    public boolean cancel()
        throws DaoException, IOException, SQLException {
        int ans = JOptionPane.showConfirmDialog(mFrame,
                                                "Would you like to cancel simulation job?",
                                                "Cancel simulation?",
                                                JOptionPane.YES_NO_OPTION);

        if (ans != JOptionPane.YES_OPTION)
            return false;
        Job job = mSimulationDao.obtainJob(mTaskId, mJobId);
        return job.cancel();
    }

    public void sendViaGaruda()
        throws DaoException, GarudaConnectionNotInitializedException, IOException, SQLException {
        File isdFile = mSimulationDao.obtainJob(mTaskId, mJobId).getIsdFile();
        GadgetDialog dialog = new GadgetDialog(mFrame, this, isdFile);
        dialog.setLocationRelativeTo(mFrame);
        GarudaClient.requestForLoadableGadgets(dialog, "csv");
    }

    public void export()
        throws DaoException, IOException, SQLException {
        TaskDao taskDao = mSimulationDao.obtainTask(mTaskId);
        File isdFile = taskDao.obtainJob(mJobId).getIsdFile();
        InputDialogForExport inputDialog = new InputDialogForExport(mFrame);
        String ext = inputDialog.show();
        if (ext == null)
            return;

        File modelFile = new File(taskDao.getModelPath());
        String baseName = Utility.getFileName(modelFile.getName());
        File defaultFile = new File(modelFile.getParent(),
                                    baseName + "_" + mJobId + "." + ext);
        FileChooser fileChooser = new FileChooser(mFrame,
                                                  "Export file", FileChooser.Mode.SAVE, defaultFile);

        if (fileChooser.showDialog()) {
            final File file = fileChooser.getSelectedFile();
            if (file.exists()) {
                int ans = JOptionPane.showConfirmDialog(mFrame,
                                                        "Is it OK to replace the existing file?",
                                                        "Replace the existing file?",
                                                        JOptionPane.YES_NO_OPTION);
                if (ans != JOptionPane.YES_OPTION)
                    return;
            }
            if ("csv".equalsIgnoreCase(ext)) { // export as CSV
                ExportReceiver receiver = new ExportReceiver(mFrame);
                final ExportWorker worker = new ExportWorker((IFrame)mFrame, receiver, isdFile, file);
                receiver.setWorker(worker); // make cancellation possible
                worker.addPropertyChangeListener(new PropertyChangeListener() {
                    @Override
                    public void propertyChange(PropertyChangeEvent evt) {
                        String propertyName = evt.getPropertyName();
                        Object newValue = evt.getNewValue();
                        if ("state".equals(propertyName)
                            && SwingWorker.StateValue.DONE.equals(newValue)) {
                            try {
                                if (worker.get())
                                    showMessageDialog("Exported successfully to " + file.getPath(),
                                                      "CSV Exported");
                            } catch (CancellationException ce) {
                                showMessageDialog("Exporting is cancelled.",
                                                  "Export cancelled");
                            } catch (InterruptedException ex) {
                                showErrorDialog("Export interrupted\n\n" + ex.getMessage(),
                                                "CSV Export interrupted");
                            } catch (ExecutionException ex) {
                                showErrorDialog("Export aborted\n\n" + ex.getMessage(),
                                                "CSV Export aborted ");
                            }
                        }
                    }
                });
                worker.execute();
            } else {
                // export as ISD
                Workspace.publishFile(isdFile, file);
                showMessageDialog("Exported successfully to " + file.getPath(),
                                  "ISD Exported");
            }
        }
    }

    public void view()
        throws DaoException, IOException, SQLException {
        TaskDao taskDao = mSimulationDao.obtainTask(mTaskId);
        Job job = taskDao.obtainJob(mJobId);
        File trackFile = taskDao.getTrackFile();
        try (FileInputStream fis = new FileInputStream(trackFile)) {
            Ipc.SimulationTrack st = Ipc.SimulationTrack.parseFrom(fis);
            PlotWindow plotWindow = new PlotWindow(new File(taskDao.getModelPath()), job.getParameterDescription(), taskDao, mJobId);
            plotWindow.setLocationRelativeTo(mFrame);
            plotWindow.setVisible(true);
            plotWindow.processSimulationTrack(st);
            plotWindow.renderPlot();
        }
    }

    @Override
    public void appendLog(String s) {
        // TODO
    }

    @Override
    public void showErrorDialog(String message, String title) {
        JOptionPane.showMessageDialog(mFrame,
                                      message,
                                      title,
                                      JOptionPane.ERROR_MESSAGE);
    }

    @Override
    public void showErrorDialog(ByteString message, String title) {
        showErrorDialog(message.toStringUtf8(), title);
    }

    private void showMessageDialog(String message, String title) {
        JOptionPane.showMessageDialog(mFrame,
                                      message,
                                      title,
                                      JOptionPane.INFORMATION_MESSAGE);
    }
}
