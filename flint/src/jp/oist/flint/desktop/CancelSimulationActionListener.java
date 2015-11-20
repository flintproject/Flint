/* -*- Mode: Java; tab-width: 4; indent-tabs-mode: nil; c-basic-offset: 4 -*- vim:set ts=4 sw=4 sts=4 et: */
package jp.oist.flint.desktop;

import jp.oist.flint.dao.DaoException;
import jp.oist.flint.dao.SimulationDao;
import jp.oist.flint.dao.TaskDao;
import jp.oist.flint.form.ProgressCell;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.io.File;
import java.io.IOException;
import java.sql.SQLException;
import javax.swing.JOptionPane;

public class CancelSimulationActionListener implements ActionListener {

    private final SimulationDao mSimulationDao;

    private final ProgressCell mCell;

    public CancelSimulationActionListener(SimulationDao simulationDao, ProgressCell cell) {
        mSimulationDao = simulationDao;
        mCell = cell;
    }

    @Override
    public void actionPerformed(ActionEvent ae) {
        if (cancel()) {
            int progress = mCell.getStatusBarProgress();
            mCell.progressFinished("finished", progress);
        }
    }

    /*
     * Suppossed to be called in EDT.
     */
    private boolean cancel() {
        File file = mCell.getDocument().getFile();
        try {
            TaskDao taskDao = mSimulationDao.obtainTask(file.getPath());
            int ans = JOptionPane.showConfirmDialog(mCell,
                    "Would you like to cancel simulation?",
                    "Cancel simulation?",
                    JOptionPane.YES_NO_OPTION);
            if (ans != JOptionPane.YES_OPTION)
                return false;
            taskDao.cancel();
            return true;
        } catch (DaoException | IOException | SQLException ex) {
            JOptionPane.showMessageDialog(mCell,
                                          "Cancellation failed\n\n" + ex.getMessage(),
                                          "Cancellation failed",
                                          JOptionPane.ERROR_MESSAGE);
            return false;
        }
    }
}
