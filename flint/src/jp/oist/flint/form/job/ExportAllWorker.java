/* -*- Mode: Java; tab-width: 4; indent-tabs-mode: nil; c-basic-offset: 4 -*- vim:set ts=4 sw=4 sts=4 et: */

package jp.oist.flint.form.job;

import java.awt.Container;
import java.io.BufferedWriter;
import java.io.File;
import java.io.FileWriter;
import java.io.IOException;
import java.util.Map;
import javax.swing.BorderFactory;
import javax.swing.JCheckBox;
import javax.swing.JDialog;
import javax.swing.JLabel;
import javax.swing.JOptionPane;
import javax.swing.SwingWorker;
import jp.oist.flint.dao.JobDao;
import jp.oist.flint.dao.TaskDao;
import jp.oist.flint.filesystem.Workspace;
import jp.oist.flint.util.Utility;


public class ExportAllWorker extends SwingWorker<Void, Void> {

    private final Container mParent;

    private final TaskDao mTaskDao;

    private final File mOutputDirectory;

    private final JCheckBox mDontShowAgain;

    private Integer mLastChosenOption = JOptionPane.NO_OPTION;

    private boolean mIsExportingCancelled = false;

    public ExportAllWorker (Container parent, File outputDir, TaskDao taskDao)
            throws IOException {
        if (!outputDir.isDirectory()) 
            throw new IOException(String.format("%s is not a directory.", outputDir.getName()));

        mParent = parent;
        mTaskDao = taskDao;
        mOutputDirectory = outputDir;
        mDontShowAgain = new JCheckBox("Do not show this message again.");
        mDontShowAgain.setSelected(false);
    }

    @Override
    protected Void doInBackground() throws IOException {

        File listFile = new File(mOutputDirectory, "simulation.txt");

        if (listFile.exists()) {
            int result = showConfirmOverwriteDialog(listFile.getName());
            switch (result) {
            case JOptionPane.CANCEL_OPTION:
                mIsExportingCancelled = true;
                return null;
            case JOptionPane.NO_OPTION:
                listFile = null;
                break;
            case JOptionPane.YES_OPTION:
                listFile.delete();
                break;
            }
        } 

        BufferedWriter writer = null;
        if (listFile != null)  { 
            listFile.createNewFile();
            writer = new BufferedWriter(new FileWriter(listFile));
        }

        try {
            String baseName = Utility.getFileName(mTaskDao.getModelFile().getName());
            int jobCount = mTaskDao.getCount();
            for (int ji=1; ji<=jobCount; ji++) {
                JobDao job = mTaskDao.obtainJob(ji);

                int numberOfDigits = String.valueOf(jobCount).getBytes().length;
                File publishFile = createPublishFile(baseName, ji, numberOfDigits);

                if (publishFile.exists()) {
                    int result = showConfirmOverwriteDialog(publishFile.getName());
                    switch (result) {
                    case JOptionPane.CANCEL_OPTION:
                        mIsExportingCancelled = true;
                        return null;
                    case JOptionPane.NO_OPTION: continue;
                    case JOptionPane.YES_OPTION:
                    }
                }
                Workspace.publishFile(job.getIsdFile(), publishFile);
                int progress = (int)((double)ji / (double)jobCount * 100.0);
                setProgress(progress);

                String line = createLine(publishFile.getName(), job.getCombination());
                if (writer != null) writer.write(line);
            }
        } finally {
            if (writer != null) {
                writer.flush();
                writer.close();
            }
        }

        return null;
    }

    @Override
    protected void done () {
    }

    public boolean isExportingCancelled () {
        return mIsExportingCancelled;
    }

    public File getExportedDirecotry () {
        return mOutputDirectory;
    }

    private File createPublishFile (String baseName, int index, int numberOfDigits) {
        String format = "%s_%0" + numberOfDigits + "d.isd";
        String filename = String.format(format, baseName, index);

        return new File(mOutputDirectory, filename);
    }

    private String createLine (String fileName, Map<String, Number> parameters) {
        StringBuilder sb = new StringBuilder(fileName).append(",");

        for (Map.Entry<String, Number> entry : parameters.entrySet()) {
            sb.append(entry.getKey())
                    .append("=")
                    .append(entry.getValue())
                    .append(",");
        }
        int lastIndex = sb.length()-1;
        return sb.substring(0, lastIndex) + System.getProperty("line.separator");
    }

    private int showConfirmOverwriteDialog (String fileName) {
        if (mDontShowAgain.isSelected()) 
            return mLastChosenOption;

        JLabel msg1 = new JLabel(fileName + " already exists.");
        JLabel msg2 = new JLabel("Do you want to overwrite it?");

        msg1.setBorder(BorderFactory.createEmptyBorder(0, 5, 0, 0));
        msg2.setBorder(BorderFactory.createEmptyBorder(0, 5, 20, 0));

        String title = "File exists";

        JOptionPane optionPane = new JOptionPane(new Object[] {msg1, msg2, mDontShowAgain},
                JOptionPane.QUESTION_MESSAGE, JOptionPane.YES_NO_CANCEL_OPTION);

        JDialog dialog = optionPane.createDialog(mParent, title);
        dialog.setModalityType(JDialog.ModalityType.APPLICATION_MODAL);
        dialog.setVisible(true);

        mLastChosenOption = (Integer)optionPane.getValue();
        return mLastChosenOption;
    }
}
