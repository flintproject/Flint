/* -*- Mode: Java; tab-width: 4; indent-tabs-mode: nil; c-basic-offset: 4 -*- vim:set ts=4 sw=4 sts=4 et: */
package jp.oist.flint.desktop;

import jp.oist.flint.dao.DaoException;
import jp.oist.flint.dao.TaskDao;
import jp.oist.flint.executor.PhspSimulator;
import jp.oist.flint.form.ProgressPane;
import jp.oist.flint.textformula.analyzer.ParseException;
import java.io.IOException;
import java.sql.SQLException;
import java.util.List;
import javax.swing.SwingWorker;
import javax.xml.parsers.ParserConfigurationException;
import javax.xml.transform.TransformerException;

class ParametrizationMonitor extends SwingWorker<Void, TaskDao> {

    private final PhspSimulator mSimulator;

    private final ProgressPane mProgressPane;

    public ParametrizationMonitor(PhspSimulator simulator, ProgressPane progressPane) {
        mSimulator = simulator;
        mProgressPane = progressPane;
    }

    @Override
    protected Void doInBackground() {
        for (;;) {
            try {
                Thread.sleep(100);
            } catch (InterruptedException ex) {
            }

            try {
                boolean done = true;
                for (int i : mSimulator.getSimulationDao().getTaskIdSet()) {
                    TaskDao task = mSimulator.getSimulationDao().obtainTask(i);
                    if (!task.isStarted()) {
                        done = false;
                        continue;
                    }
                    publish(task);
                }
                if (done)
                    return null;
            } catch (DaoException | IOException | SQLException ex) {
                // let's wait and retry
            }
        }
    }

    @Override
    protected void process(List<TaskDao> tasks) {
        for (TaskDao task : tasks) {
            try {
                mProgressPane.prepareWindow(mSimulator, task.getModelPath(), task.getCount());
            } catch (DaoException |
                     IOException |
                     ParseException |
                     ParserConfigurationException |
                     TransformerException ex) {
                // skip
            }
        }
    }
}
