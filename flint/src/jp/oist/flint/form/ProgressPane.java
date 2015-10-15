/* -*- Mode: Java; tab-width: 4; indent-tabs-mode: nil; c-basic-offset: 4 -*- vim:set ts=4 sw=4 sts=4 et: */
package jp.oist.flint.form;

import jp.oist.flint.desktop.Document;
import jp.oist.flint.desktop.IDesktopListener;
import jp.oist.flint.desktop.ISimulationListener;
import jp.oist.flint.executor.PhspSimulator;
import jp.oist.flint.form.sub.SubFrame;
import java.io.File;
import java.io.IOException;
import java.util.ArrayList;
import javax.swing.BoxLayout;
import javax.swing.JPanel;
import javax.swing.JScrollPane;
import javax.xml.parsers.ParserConfigurationException;

public class ProgressPane extends PeripheralPane
    implements IDesktopListener, ISimulationListener {

    private final ArrayList<ProgressCell> mCells;

    private final JPanel mPanel;

    private final JScrollPane mScrollPane;

    public ProgressPane() {
        mCells = new ArrayList<>();

        mPanel = new JPanel();
        mPanel.setLayout(new BoxLayout(mPanel, BoxLayout.PAGE_AXIS));

        mScrollPane = new JScrollPane(mPanel);
        setContentPane(mScrollPane);
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

    public ProgressCell getListCellOfModel (File modelFile) {
        for (ProgressCell cell : mCells) {
            if (cell.getDocument().getFile().equals(modelFile))
                return cell;
        }
        return null;
    }

    public void prepareJobWindows(PhspSimulator simulator) throws IOException, ParserConfigurationException {
        for (ProgressCell cell : mCells)
            cell.prepareJobWindow(simulator);
    }

    /* IDesktopListener */

    @Override
    public void documentAdded(Document doc) {
        final SubFrame subFrame = doc.getSubFrame();
        ProgressCell cell = new ProgressCell(doc);
        cell.addMouseListener(new ProgressCellMouseListener(subFrame));
        mCells.add(cell);
        mPanel.add(cell);
        mPanel.repaint();
        setSelectedCell(cell, true);
        subFrame.setStatusComponent(cell);
    }

    @Override
    public void documentRemoved(Document doc, boolean empty) {
        for (ProgressCell cell : mCells) {
            if (doc.equals(cell.getDocument())) {
                mCells.remove(cell);
                mPanel.remove(cell);
                mPanel.repaint();
                break;
            }
        }
    }

    /* ISimulationListener */

    @Override
    public void simulationStarted(PhspSimulator simulator) {
        for (ProgressCell cell : mCells)
            cell.progressStarted();
        repaint();
    }

    @Override
    public void simulationDone() {
        repaint();
    }

    @Override
    public void simulationPaused() {
    }

    @Override
    public void simulationResumed() {
    }
}
