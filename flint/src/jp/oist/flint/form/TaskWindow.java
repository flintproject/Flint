/* -*- Mode: Java; tab-width: 4; indent-tabs-mode: nil; c-basic-offset: 4 -*- vim:set ts=4 sw=4 sts=4 et: */
package jp.oist.flint.form;

import jp.oist.flint.dao.SimulationDao;
import jp.oist.flint.form.job.JobCell;
import jp.oist.flint.form.sub.SubFrame;
import jp.oist.flint.task.ParameterDefinition;
import jp.oist.flint.task.Task;
import jp.oist.flint.theme.Icon;
import java.awt.BorderLayout;
import java.awt.Dimension;
import java.awt.FlowLayout;
import java.util.ArrayList;
import java.util.Timer;
import javax.swing.BorderFactory;
import javax.swing.JButton;
import javax.swing.JComboBox;
import javax.swing.JFrame;
import javax.swing.JPanel;

public class TaskWindow extends JFrame
    implements IJobMenuProvider {

    static final int MIN_WIDTH = 600;

    static final int MIN_HEIGHT = 250;

    static final String TIMER_NAME = "TaskWindow's";

    static final long DELAY = 250;

    private final Task mTask;

    private final SimulationDao mSimulationDao;

    private final ArrayList<JComboBox> mComboBoxes = new ArrayList<>();

    private Timer mTimer;

    private JobCell mCell;

    public TaskWindow(SubFrame subFrame, String title, Task task, SimulationDao simulationDao) {
        super(title);

        mTask = task;
        mSimulationDao = simulationDao;

        setIconImage(Icon.getImage());

        /* north */
        JPanel northPanel = new JPanel();
        northPanel.setLayout(new FlowLayout(FlowLayout.RIGHT));
        JButton buttonExportAll = new JButton("Export All");
        buttonExportAll.addActionListener(new ExportAllActionListener(this, mSimulationDao, task.getId()));
        northPanel.add(buttonExportAll);
        add(northPanel, BorderLayout.NORTH);

        /* center */
        mCell = new JobCell(this, 0);
        add(mCell, BorderLayout.CENTER);

        /* south */
        JPanel southPanel = new JPanel();
        for (int i = 0; i < mTask.getNumberOfParameters(); i++) {
            ParameterDefinition pd = mTask.getParameterDefinition(i);
            JPanel panel = new JPanel();
            panel.setBorder(BorderFactory.createTitledBorder(pd.getName()));
            JComboBox comboBox = new JComboBox(pd.getValues());
            if (pd.getName().length() >= 5) {
                 // to reserve the width for the title
                comboBox.setPrototypeDisplayValue(pd.getName());
            }
            comboBox.addActionListener(new ParameterComboBoxActionListener(this));
            mComboBoxes.add(comboBox);
            panel.add(comboBox);
            southPanel.add(panel);
        }
        add(southPanel, BorderLayout.SOUTH);

        setMinimumSize(new Dimension(MIN_WIDTH, MIN_HEIGHT));
        setPreferredSize(new Dimension(MIN_WIDTH, MIN_HEIGHT));
        pack();

        mTimer = new Timer(TIMER_NAME, true);
        mTimer.scheduleAtFixedRate(new JobCellTimerTask(mTimer, mCell, mSimulationDao, mTask.getId(), 1), 0, DELAY);

        CloseByKeyStrokeAction.register(this, subFrame);
    }

    /*
     * Supposed to be called in EDT.
     */
    public void changeCell() {
        int index = 0;
        int p = 1;
        for (JComboBox cb : mComboBoxes) {
            int i = cb.getSelectedIndex();
            if (i < 0)
                return; // ignored
            index += i * p;
            p *= cb.getItemCount();
        }

        mTimer.cancel();
        remove(mCell);

        mCell = new JobCell(this, index);
        add(mCell, BorderLayout.CENTER);
        repaint();

        mTimer = new Timer(TIMER_NAME, true);
        mTimer.scheduleAtFixedRate(new JobCellTimerTask(mTimer, mCell, mSimulationDao, mTask.getId(), index+1), 0, DELAY);
    }

    @Override
    public JobMenu getJobMenu(int index) {
        return new JobMenu(this, mSimulationDao, mTask.getId(), index+1);
    }
}
