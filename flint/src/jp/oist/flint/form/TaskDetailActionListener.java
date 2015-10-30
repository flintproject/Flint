/* -*- Mode: Java; tab-width: 4; indent-tabs-mode: nil; c-basic-offset: 4 -*- vim:set ts=4 sw=4 sts=4 et: */
package jp.oist.flint.form;

import jp.oist.flint.dao.DaoException;
import jp.oist.flint.dao.SimulationDao;
import jp.oist.flint.dao.TaskDao;
import jp.oist.flint.executor.PhspSimulator;
import jp.oist.flint.form.sub.SubFrame;
import jp.oist.flint.task.ParameterDefinitionException;
import jp.oist.flint.task.Task;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.io.IOException;
import java.sql.SQLException;
import javax.swing.JOptionPane;

class TaskDetailActionListener implements ActionListener {

    private final SimulationDao mSimulationDao;

    private final SubFrame mSubFrame;

    public TaskDetailActionListener(PhspSimulator simulator, SubFrame subFrame) {
        mSimulationDao = simulator.getSimulationDao();
        mSubFrame = subFrame;
    }

    @Override
    public void actionPerformed(ActionEvent ae) {
        try {
            TaskDao taskDao = mSimulationDao.obtainTask(mSubFrame.getDocument().getFile().getPath());
            Task task = new Task(taskDao.getTaskId(), taskDao.getWorkingDir());
            TaskWindow window = new TaskWindow(taskDao.getModelPath(), task, mSimulationDao);
            window.setLocationRelativeTo(mSubFrame);
            window.setVisible(true);
        } catch (DaoException |
                 IOException |
                 ParameterDefinitionException |
                 SQLException ex) {
            JOptionPane.showMessageDialog(mSubFrame,
                                          ex.getMessage(),
                                          "Error on showing detail",
                                          JOptionPane.ERROR_MESSAGE);
        }
    }
}
