/* -*- Mode: Java; tab-width: 4; indent-tabs-mode: nil; c-basic-offset: 4 -*- vim:set ts=4 sw=4 sts=4 et: */
package jp.oist.flint.form;

import jp.oist.flint.dao.DaoException;
import jp.oist.flint.dao.SimulationDao;
import jp.oist.flint.form.job.JobCell;
import java.io.IOException;
import java.sql.SQLException;
import java.util.Timer;
import java.util.TimerTask;

class JobCellTimerTask extends TimerTask {

    private final Timer mTimer;

    private final JobCell mCell;

    private final SimulationDao mSimulationDao;

    private final int mTaskId;

    private final int mJobId;

    public JobCellTimerTask(Timer timer, JobCell cell, SimulationDao simulationDao,
                            int taskId, int jobId) {
        mTimer = timer;
        mCell = cell;
        mSimulationDao = simulationDao;
        mTaskId = taskId;
        mJobId = jobId;
    }

    @Override
    public void run() {
        try {
            boolean b = mCell.setProgress(mSimulationDao.obtainJob(mTaskId, mJobId));
            mCell.repaint();
            if (b)
                mTimer.cancel();
        } catch (DaoException | IOException | SQLException ex) {
            // ignored
        }
    }
}
