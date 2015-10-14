/* -*- Mode: Java; tab-width: 4; indent-tabs-mode: nil; c-basic-offset: 4 -*- vim:set ts=4 sw=4 sts=4 et: */
package jp.oist.flint.form;

import java.util.ArrayList;
import java.util.Collections;
import java.util.List;
import javax.swing.BoxLayout;
import javax.swing.JPanel;
import javax.swing.JScrollPane;

class ProgressList {

    private final ArrayList<ProgressCell> mCells;

    private final JPanel mPanel;

    private final JScrollPane mScrollPane;

    public ProgressList() {
        mCells = new ArrayList<>();

        mPanel = new JPanel();
        mPanel.setLayout(new BoxLayout(mPanel, BoxLayout.PAGE_AXIS));

        mScrollPane = new JScrollPane(mPanel);
    }

    public List<ProgressCell> getCells() {
        return Collections.unmodifiableList(mCells);
    }

    public void add(ProgressCell cell) {
        mCells.add(cell);
        mPanel.add(cell);
        mPanel.repaint();
    }

    public void remove(ProgressCell cell) {
        mCells.remove(cell);
        mPanel.remove(cell);
        mPanel.repaint();
    }

    public JScrollPane createPane() {
        return mScrollPane;
    }

    public void setSelectedCell(ProgressCell cell, boolean selected) {
        if (!selected) {
            cell.setSelected(false);
            return;
        }
        for (ProgressCell other : mCells) {
            if (cell == other) {
                other.setSelected(true);
            } else {
                other.setSelected(false);
            }
        }
    }
}
