/* -*- Mode: Java; tab-width: 4; indent-tabs-mode: nil; c-basic-offset: 4 -*- vim:set ts=4 sw=4 sts=4 et: */
package jp.oist.flint.desktop;

import jp.oist.flint.dao.DaoException;
import jp.oist.flint.dao.TaskDao;
import jp.oist.flint.executor.PhspProgressMonitor;
import jp.oist.flint.executor.PhspSimulator;
import jp.oist.flint.form.ProgressPane;
import jp.oist.flint.textformula.analyzer.ParseException;
import java.io.IOException;
import java.sql.SQLException;
import java.util.List;
import java.util.HashSet;
import javax.swing.SwingWorker;
import javax.xml.parsers.ParserConfigurationException;
import javax.xml.transform.TransformerException;

class ParametrizationMonitor extends SwingWorker<Void, TaskDao> {

    private final PhspSimulator mSimulator;

    private final ProgressPane mProgressPane;

    private final PhspProgressMonitor mProgressMonitor;

    public ParametrizationMonitor(PhspSimulator simulator,
                                  ProgressPane progressPane,
                                  PhspProgressMonitor progressMonitor) {
        mSimulator = simulator;
        mProgressPane = progressPane;
        mProgressMonitor = progressMonitor;
    }

    @Override
    protected Void doInBackground() {
        HashSet<Integer> taskIdSet = new HashSet<>();
        while (taskIdSet.isEmpty()) {
            try {
                Thread.sleep(1000);
            } catch (InterruptedException ex) {
            }

            try {
                taskIdSet.addAll(mSimulator.getSimulationDao().getTaskIdSet());
            } catch (DaoException | IOException | SQLException ex) {
            }
        }
        mProgressMonitor.execute();
        while (!taskIdSet.isEmpty()) {
            HashSet<Integer> done = new HashSet<>();
            for (int i : taskIdSet) {
                try {
                    TaskDao task = mSimulator.getSimulationDao().obtainTask(i);
                    if (!task.isStarted())
                        continue;
                    publish(task);
                    done.add(i);
                } catch (DaoException | IOException | SQLException ex) {
                    // let's wait and retry
                }
            }
            taskIdSet.removeAll(done);

            try {
                Thread.sleep(1000);
            } catch (InterruptedException ex) {
            }
        }
        return null;
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
