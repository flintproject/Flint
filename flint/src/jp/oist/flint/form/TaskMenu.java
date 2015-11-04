/* -*- Mode: Java; tab-width: 4; indent-tabs-mode: nil; c-basic-offset: 4 -*- vim:set ts=4 sw=4 sts=4 et: */
package jp.oist.flint.form;

import jp.oist.flint.control.DirectoryChooser;
import jp.oist.flint.dao.DaoException;
import jp.oist.flint.dao.TaskDao;
import jp.oist.flint.filesystem.Filename;
import jp.oist.flint.form.job.ExportAllWorker;
import jp.oist.flint.form.sub.ConfirmDialogForOverwritingFile;
import jp.oist.flint.form.sub.InputDialogForExport;
import jp.oist.flint.job.Job;
import java.awt.HeadlessException;
import java.beans.PropertyChangeEvent;
import java.beans.PropertyChangeListener;
import java.io.File;
import java.io.IOException;
import java.nio.charset.StandardCharsets;
import java.sql.SQLException;
import java.util.ArrayList;
import java.util.concurrent.CancellationException;
import java.util.concurrent.ExecutionException;
import javax.swing.JFrame;
import javax.swing.JOptionPane;
import javax.swing.SwingWorker;

public class TaskMenu implements IFrame {

    private final JFrame mFrame;

    private final TaskDao mTaskDao;

    public TaskMenu(JFrame frame, TaskDao taskDao) {
        mFrame = frame;
        mTaskDao = taskDao;
    }

    public void exportAll() throws DaoException, IOException, SQLException {
        InputDialogForExport inputDialog = new InputDialogForExport(mFrame);
        String extension = inputDialog.show();
        if (extension == null)
            return;

        String defaultDir = System.getProperty("user.home");
        DirectoryChooser chooser = new DirectoryChooser(mFrame, "Choose a target directory", defaultDir);
        if (!chooser.showDialog())
            return;

        final File selectedDir = chooser.getSelectedDirectory();

        if (!selectedDir.exists()) {
            int result = JOptionPane.showConfirmDialog(mFrame,
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

        ConfirmDialogForOverwritingFile confirmDialog = new ConfirmDialogForOverwritingFile(mFrame);
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
        int numJobs = mTaskDao.getCount();
        int numDigits = String.valueOf(numJobs).getBytes(StandardCharsets.UTF_8).length;
        for (int i=1; i<=numJobs; i++) {
            Job job = mTaskDao.obtainJob(i);
            File isdFile = job.getIsdFile();
            File targetFile = Filename.getVariant(new File(mTaskDao.getModelPath()), selectedDir, i, numDigits, extension);
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
        }

        final ExportAllWorker worker = new ExportAllWorker(this,
                                                           mFrame,
                                                           extension,
                                                           listFile,
                                                           isdFiles,
                                                           targetFiles);

        worker.addPropertyChangeListener(new PropertyChangeListener() {
            @Override
            public void propertyChange(PropertyChangeEvent evt) {
                String propertyName = evt.getPropertyName();
                Object nv = evt.getNewValue();

                if ("state".equals(propertyName)
                    && SwingWorker.StateValue.STARTED.equals(nv)) {
                    mFrame.setEnabled(false);
                } else if ("state".equals(propertyName)
                           && SwingWorker.StateValue.DONE.equals(nv)) {
                    mFrame.setEnabled(true);
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

    private void showMessageDialog(String message, String title) {
        JOptionPane.showMessageDialog(mFrame,
                                      message,
                                      title,
                                      JOptionPane.INFORMATION_MESSAGE);
    }
}
