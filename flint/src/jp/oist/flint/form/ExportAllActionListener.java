/* -*- Mode: Java; tab-width: 4; indent-tabs-mode: nil; c-basic-offset: 4 -*- vim:set ts=4 sw=4 sts=4 et: */
package jp.oist.flint.form;

import jp.oist.flint.dao.DaoException;
import jp.oist.flint.dao.SimulationDao;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.io.IOException;
import java.sql.SQLException;
import javax.swing.JFrame;
import javax.swing.JOptionPane;

class ExportAllActionListener implements ActionListener {

    private final JFrame mFrame;

    private final SimulationDao mSimulationDao;

    private final int mTaskId;

    public ExportAllActionListener(JFrame frame, SimulationDao simulationDao, int taskId) {
        mFrame = frame;
        mSimulationDao = simulationDao;
        mTaskId = taskId;
    }

    @Override
    public void actionPerformed(ActionEvent ae) {
        try {
            TaskMenu menu = new TaskMenu(mFrame, mSimulationDao.obtainTask(mTaskId));
            menu.exportAll();
        } catch (DaoException |
                 IOException |
                 SQLException ex) {
            JOptionPane.showMessageDialog(mFrame,
                                          ex.getMessage(),
                                          "Error on exporting all",
                                          JOptionPane.ERROR_MESSAGE);
        }
    }
}
