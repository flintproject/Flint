/* -*- Mode: Java; tab-width: 4; indent-tabs-mode: nil; c-basic-offset: 4 -*- vim:set ts=4 sw=4 sts=4 et: */
package jp.oist.flint.form;

import jp.oist.flint.form.job.JobCell;
import jp.oist.flint.form.sub.JobWindow;
import jp.oist.flint.job.Job;
import java.awt.BorderLayout;
import java.util.ArrayList;
import javax.swing.BoxLayout;
import javax.swing.JPanel;
import javax.swing.JScrollPane;

public class JobPane extends JPanel {

    private final ArrayList<JobCell> mCells;

    private final JPanel mPanel;

    public JobPane() {
        super(new BorderLayout());

        mCells = new ArrayList<>();

        mPanel = new JPanel();
        mPanel.setLayout(new BoxLayout(mPanel, BoxLayout.PAGE_AXIS));

        JScrollPane scrollPane = new JScrollPane(mPanel);
        add(scrollPane);
    }

    private void add(JobCell cell) {
        mCells.add(cell);
        mPanel.add(cell);
        mPanel.repaint();
    }

    public synchronized void setProgress(JobWindow window, int index, Job job) {
        JobCell cell;
        if (index < mCells.size()) {
            cell = mCells.get(index);
        } else {
            int i = mCells.size();
            do {
                cell = new JobCell(window, i++);
                add(cell);
            } while (i <= index);
        }
        cell.setProgress(job);
        repaint();
    }

    public synchronized int getProgress(int index) {
        if (index < mCells.size()) {
            return mCells.get(index).getProgress();
        }
        return 0;
    }

    public synchronized boolean isCancelled(int index) {
        if (index < mCells.size()) {
            return mCells.get(index).isCancelled();
        }
        return false;
    }

    public synchronized void setCancelled(int index, boolean cancelled) {
        if (index < mCells.size()) {
            mCells.get(index).setCancelled(cancelled);
        }
    }

}
