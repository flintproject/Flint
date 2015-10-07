/* -*- Mode: Java; tab-width: 4; indent-tabs-mode: nil; c-basic-offset: 4 -*- vim:set ts=4 sw=4 sts=4 et: */
package jp.oist.flint.executor;

import jp.oist.flint.dao.DaoException;
import jp.oist.flint.dao.SimulationDao;
import jp.oist.flint.dao.TaskDao;
import jp.oist.flint.form.MainFrame;
import jp.oist.flint.form.ProgressCell;
import jp.oist.flint.form.ProgressPane;
import jp.oist.flint.form.job.IProgressManager;
import jp.oist.flint.form.sub.SubFrame;
import jp.oist.flint.job.Job;
import jp.oist.flint.job.Progress;
import java.io.File;
import java.io.IOException;
import java.sql.SQLException;
import java.util.List;
import java.util.Map;
import javax.swing.SwingWorker;

public class PhspProgressMonitor extends SwingWorker<Void, Job> {

    private final SimulationDao mSimulationDao;

    private final MainFrame mMainFrame;

    private final ProgressPane mProgressPane;

    private volatile boolean mDone = false;

    public PhspProgressMonitor(SimulationDao simulationDao, MainFrame mainFrame, ProgressPane progressPane) {
        mSimulationDao = simulationDao;
        mMainFrame = mainFrame;
        mProgressPane = progressPane;
    }

    public void stop() {
        mDone = true;
    }

    /*
     * Note that this method should be called in EDT.
     */
    private void displayProgress(Job job) {
        int taskId = job.getTaskId();
        Map<String, Number> combination = job.getCombination();
        try {
            TaskDao taskDao = mSimulationDao.obtainTask(taskId);
            String modelPath = taskDao.getModelPath();
            SubFrame subFrame = mMainFrame.findSubFrame(modelPath);

            Progress progress = job.getProgress();
            IProgressManager progressMgr = subFrame.getProgressManager();
            int index = progressMgr.indexOf(combination);

            progressMgr.setProgress(index, progress);

            if (taskDao.isCancelled())
                progressMgr.setCancelled(index, true);

            int taskProgress = taskDao.getProgress();
            ProgressCell cell = mProgressPane.getListCellOfModel(new File(modelPath));

            String status;
            if (taskDao.isFinished()) {
                status = (taskDao.isCancelled())? "finished" : "completed";
                cell.progressFinished(status, 0, 100, taskProgress);
            } else if (taskDao.isStarted()) {
                status = (taskDao.isCancelled())? "cancelling..." : taskProgress + " %";
                cell.setProgress(status, 0, 100, taskProgress);
            }
        } catch (DaoException | IOException | SQLException ex) {
            // give up
        }
    }

    @Override
    protected Void doInBackground() {
        while (!mDone) {
            try {
                Thread.sleep(100);
            } catch (InterruptedException ex) {
            }

            try {
                for (int i : mSimulationDao.getTaskIdSet()) {
                    TaskDao task = mSimulationDao.obtainTask(i);
                    if (!task.isStarted())
                        continue;
                    int jobCount = task.getCount();
                    for (int j=1; j<=jobCount; j++) { // base 1
                        Job job = task.obtainJob(j);
                        publish(job);
                    }
                }
            } catch (DaoException | IOException | SQLException ex) {
                // give up
            }
        }
        return null;
    }

    @Override
    protected void done() {
        try {
            for (int i : mSimulationDao.getTaskIdSet()) {
                TaskDao task = mSimulationDao.obtainTask(i);
                if (!task.isStarted())
                    continue;
                int jobCount = task.getCount();
                for (int j=1; j<=jobCount; j++) { // base 1
                    Job job = task.obtainJob(j);
                    displayProgress(job);
                }
            }
        } catch (DaoException | IOException | SQLException ex) {
            // give up
        }
    }

    @Override
    protected void process(List<Job> jobs) {
        for (Job job : jobs)
            displayProgress(job);
    }
}
