/* -*- Mode: Java; tab-width: 4; indent-tabs-mode: nil; c-basic-offset: 4 -*- vim:set ts=4 sw=4 sts=4 et: */
package jp.oist.flint.form;

import jp.oist.flint.desktop.Document;
import jp.oist.flint.desktop.IDesktopListener;
import jp.oist.flint.desktop.ISimulationListener;
import jp.oist.flint.executor.PhspSimulator;
import jp.oist.flint.form.sub.SubFrame;
import java.awt.Container;
import java.io.File;
import javax.swing.DefaultListModel;
import javax.swing.JScrollPane;

public class ProgressPane extends PeripheralPane
    implements IDesktopListener, ISimulationListener {

    public final static String TITLE = "Progresses";

    public final static String STOP_ACTION_COMMAND      = "progress.stop";

    public final static String LOG_ACTION_COMMAND       = "progress.log";

    public final static String JOB_ACTION_COMMAND      = "progress.job";

    private ProgressList mProgressList;

    public ProgressPane() {
        super(TITLE);
        initComponents();
    }

    protected Container createContentPane () {
        mProgressList = new ProgressList();
        JScrollPane scrollPane = new JScrollPane(mProgressList);

        return scrollPane;
    }

    private void initComponents () {
        setContentPane(createContentPane());
    }

    public void addRow (ProgressCell row) {
        DefaultListModel model = (DefaultListModel)mProgressList.getModel();
        model.addElement(row);
    }

    public int getSelectedIndex () {
        return mProgressList.getSelectedIndex();
    }

    public ProgressCell getSelectedItem () {
        return (ProgressCell)mProgressList.getSelectedValue();
    }

    public void setSelectedIndex (int index) {
        mProgressList.setSelectedIndex(index);
    }

    public void setSelectedCell (ProgressCell plcp, boolean selected) {
        mProgressList.setSelectedValue(plcp, selected);
    }

    private ProgressCell[]  getListCells () {
        DefaultListModel model = (DefaultListModel)mProgressList.getModel();
        int size = model.getSize();
        ProgressCell[] retvals = new ProgressCell[size];
        for (int i=0; i<size; i++) 
            retvals[i] = (ProgressCell)model.getElementAt(i);

        return  retvals;
    }

    public ProgressCell getListCell (int index) {
        DefaultListModel model = (DefaultListModel)mProgressList.getModel();
        return (ProgressCell)model.getElementAt(index);
    }

    public int getListCellCount () {
        return mProgressList.getModel().getSize();
    }

    public void removeAllListCell () {
        DefaultListModel model = (DefaultListModel)mProgressList.getModel();
        model.removeAllElements();
    }

    public void removeListCell (int index)  {
        DefaultListModel model = (DefaultListModel)mProgressList.getModel();
        model.remove(index);
    }

    public void removeListCell (ProgressCell cell) {
        DefaultListModel model = (DefaultListModel)mProgressList.getModel();
        model.removeElement(cell);
    }

    public ProgressCell getListCellOfModel (File modelFile) {
        DefaultListModel model = (DefaultListModel)mProgressList.getModel();
        int size = model.getSize();

        for (int i=0; i<size; i++) {
            ProgressCell cell = (ProgressCell)model.getElementAt(i);
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
        addRow(plcp);
        subFrame.setStatusComponent(plcp);
    }

    @Override
    public void documentRemoved(Document doc, boolean empty) {
        ProgressCell[] cells = getListCells();
        for (ProgressCell cell : cells) {
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
