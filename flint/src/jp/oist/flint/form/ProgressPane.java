/* -*- Mode: Java; tab-width: 4; indent-tabs-mode: nil; c-basic-offset: 4 -*- vim:set ts=4 sw=4 sts=4 et: */
package jp.oist.flint.form;

import jp.oist.flint.desktop.Document;
import jp.oist.flint.desktop.IDesktopListener;
import jp.oist.flint.desktop.ISimulationListener;
import jp.oist.flint.executor.PhspSimulator;
import jp.oist.flint.form.sub.SubFrame;
import java.awt.Container;
import java.io.File;
import java.util.ArrayList;
import java.util.Collections;
import java.util.Enumeration;
import javax.swing.DefaultListModel;
import javax.swing.JScrollPane;

public class ProgressPane extends PeripheralPane
    implements IDesktopListener, ISimulationListener {

    public final static String TITLE = "Progresses";

    private final ProgressList mProgressList;

    public ProgressPane() {
        super(TITLE);
        mProgressList = new ProgressList();
        setContentPane(new JScrollPane(mProgressList));
    }

    public void setSelectedCell (ProgressCell plcp, boolean selected) {
        mProgressList.setSelectedValue(plcp, selected);
    }

    private ArrayList<ProgressCell> getListCells() {
        DefaultListModel model = (DefaultListModel)mProgressList.getModel();
        return Collections.list(model.elements());
    }

    private void removeListCell(ProgressCell cell) {
        DefaultListModel model = (DefaultListModel)mProgressList.getModel();
        model.removeElement(cell);
    }

    public ProgressCell getListCellOfModel (File modelFile) {
        for (ProgressCell cell : getListCells()) {
            if (cell.getDocument().getFile().equals(modelFile))
                return cell;
        }
        return null;
    }

    /* IDesktopListener */

    @Override
    public void documentAdded(Document doc) {
        final SubFrame subFrame = doc.getSubFrame();
        ProgressCell plcp = new ProgressCell(doc, mProgressList);
        DefaultListModel model = (DefaultListModel)mProgressList.getModel();
        model.addElement(plcp);
        subFrame.setStatusComponent(plcp);
    }

    @Override
    public void documentRemoved(Document doc, boolean empty) {
        for (ProgressCell cell : getListCells()) {
            if (doc.equals(cell.getDocument())) {
                removeListCell(cell);
                break;
            }
        }
    }

    /* ISimulationListener */

    @Override
    public void simulationStarted(PhspSimulator simulator) {
        for (ProgressCell cell : getListCells())
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
