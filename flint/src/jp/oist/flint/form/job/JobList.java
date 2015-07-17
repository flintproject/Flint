/* -*- Mode: Java; tab-width: 4; indent-tabs-mode: nil; c-basic-offset: 4 -*- vim:set ts=4 sw=4 sts=4 et: */
package jp.oist.flint.form.job;

import jp.oist.flint.job.Progress;
import java.awt.BorderLayout;
import java.awt.Component;
import java.awt.Container;
import java.awt.Point;
import java.awt.Rectangle;
import java.awt.event.MouseEvent;
import java.awt.event.MouseListener;
import java.beans.PropertyChangeEvent;
import java.beans.PropertyChangeListener;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import javax.swing.ButtonModel;
import javax.swing.JButton;
import javax.swing.JList;
import javax.swing.JPanel;
import javax.swing.JScrollPane;
import javax.swing.ListCellRenderer;
import javax.swing.ListModel;
import javax.swing.ListSelectionModel;
import javax.swing.ScrollPaneConstants;
import javax.swing.SwingUtilities;
import javax.swing.event.ListDataEvent;
import javax.swing.event.ListDataListener;

public class JobList extends JobViewerComponent 
    implements ListCellRenderer, MouseListener, ListDataListener, PropertyChangeListener {

    private JList mJobList;

    public List<JobListCell> mCells;

    public JobList () {
        super(new BorderLayout());

        initComponents();

        initEvents();
    }

    private void initComponents () {
        mJobList = new JList();

        JScrollPane scrollPane = new JScrollPane(mJobList);
        scrollPane.setHorizontalScrollBarPolicy(ScrollPaneConstants.HORIZONTAL_SCROLLBAR_NEVER);

        add(scrollPane, BorderLayout.CENTER);

        mCells = new ArrayList<>();
    }

    private void initEvents () {
        mJobList.setCellRenderer(this);
        addPropertyChangeListener(this);
        mJobList.addMouseListener(this);
    }

    protected JButton getButtonOnContainer (Container container, Point point) {
        Rectangle r = container.getBounds();
        point.translate(-r.x, -r.y);
        Component retval = SwingUtilities.getDeepestComponentAt(container, point.x, point.y);

        if (retval instanceof JButton)
            return (JButton)retval;

        return null;
    }

    protected JPanel getPanelOnList (JList list, Point point) {
        int index = list.locationToIndex(point);

        if (index < 0) return null;

        Component c = list.getCellRenderer()
                .getListCellRendererComponent(list, getValues(index), index, false, false);

        Rectangle r = list.getCellBounds(index, index);
        c.setBounds(r);

        if (c instanceof JPanel) 
            return (JPanel)c;

        return null;
    }

    @Override
    public Component getListCellRendererComponent(JList list, Object value, 
            int index, boolean isSelected, boolean cellHasFocus) {
        if (index >= mCells.size()) 
            return null;

        JobListCell c = mCells.get(index);
        CombinationModel model = (CombinationModel)getModel();
        Number[] values = model.getValues(index);
        String[] titles = getParameterInfo().getTitles();
        c.setCombination(values);
        c.setCombinationTitle(titles);

        String detail;
        if (model.getParameterIsDummy()) {
            detail = "Simulation using the default parameters.";
        } else {
            StringBuilder sb = new StringBuilder();
            for (int i=0 ; i<values.length; i++)
                sb.append(String.format("%s=%s ",  titles[i], values[i]));
            detail = sb.toString();
        }
        c.setDetail(detail);

        if (isSelected) {
            c.setForeground(list.getSelectionForeground());
            c.setBackground(list.getSelectionBackground());
        } else {
            c.setForeground(list.getForeground());
            c.setBackground(list.getBackground());
        }

        c.setEnabled(list.isEnabled());

        return c;
    }

    @Override
    public void propertyChange(PropertyChangeEvent evt) {
        String propertyName = evt.getPropertyName();
        if (propertyName == null)
            return;
        Object newValue = evt.getNewValue();
        Object oldValue = evt.getOldValue();

        switch (propertyName) {
        case "model":
            if (oldValue != null)
                ((ListModel)oldValue).removeListDataListener(this);
            if (newValue != null)
                ((ListModel)newValue).addListDataListener(this);
            mJobList.setModel((ListModel)newValue);
            break;
        case "selectionModel":
            mJobList.setSelectionModel((ListSelectionModel)newValue);
            break;
        }
    }

    @Override
    public void intervalAdded (ListDataEvent evt) {
        int start = evt.getIndex0();
        int end   = evt.getIndex1();
        for (int i=start; i<=end; i++)
            mCells.add(new JobListCell(this, i));
    }

    @Override
    public void intervalRemoved (ListDataEvent evt) {
        int start = evt.getIndex0();
        int end   = evt.getIndex1();
        for (int i=end; start<=i; i--)
            mCells.remove(i);
    }

    @Override
    public void contentsChanged (ListDataEvent evt) {
    }

    @Override
    public boolean getValueIsAdjusting (int index) {
        return false;
    }

    @Override
    public void setValueIsAdjusting (int index, boolean isAdjusting) {
        if (index < 0 || mCells.size() <= index)
            return;

        mCells.get(index).setValueIsAdjusting(isAdjusting);
    }

    @Override
    public int getProgress (int index) {
        return mCells.get(index).getProgress();
    }

    @Override
    public void setProgress(int index, Progress progress) {
        if (0<=index && index < mCells.size())
            mCells.get(index).setProgress(progress);
    }

    @Override
    public boolean isCancelled (int index) {
        return mCells.get(index).isCancelled();
    }

    @Override
    public void setCancelled (int index, boolean cancelled) {
        mCells.get(index).setCancelled(cancelled);
    }

    @Override
    public void ensureIndexIsVisible (int index) {
        mJobList.ensureIndexIsVisible(index);
    }

    @Override
    public int locationToIndex (Point p) {
        return mJobList.locationToIndex(p);
    }

    @Override
    public Map<Integer, Number> getValuesAt (Point p) {
        JPanel pane = getPanelOnList(mJobList, p);

        if (pane instanceof JobListCell) {
            int index = mCells.indexOf(pane);

            if (index == -1)
                return null;

            Number[] values = getValues(index);

            if (values == null)
                return null;

            Map<Integer, Number> ret = new HashMap<>();
            for (int i=0; i<values.length; i++)
                ret.put(i, values[i]);

            return ret;
        }
        return null;
    }

    @Override
    public Map<Integer, Number> getValuesAtHover (Point p) {
        return getValuesAt(p);
    }

    @Override
    public void mouseClicked (MouseEvent evt) {
        JList list = (JList)evt.getSource();
        JPanel pane = getPanelOnList(list, evt.getPoint());
        if (pane instanceof JobListCell) {
            JButton btn = getButtonOnContainer(pane, evt.getPoint());
            if (btn == null)
              return;
            btn.doClick();
        }
    }

    @Override
    public void mousePressed (MouseEvent evt) {
        JList list = (JList)evt.getSource();
        JPanel pane = getPanelOnList(list, evt.getPoint());
        if (pane instanceof JobListCell) {
            JButton btn = getButtonOnContainer(pane, evt.getPoint());
            if (btn == null)
              return;
            ButtonModel bm = btn.getModel();
            bm.setArmed(true);
            bm.setPressed(true);
            bm.setSelected(true);
            bm.setRollover(true);
            repaint();
        }
    }

    @Override
    public void mouseReleased (MouseEvent evt) {
        JList list = (JList)evt.getSource();
        JPanel pane = getPanelOnList(list, evt.getPoint());
        if (pane instanceof JobListCell) {
            JButton btn = getButtonOnContainer(pane, evt.getPoint());
            if (btn == null)
              return;
            ButtonModel bm = btn.getModel();
            bm.setArmed(false);
            bm.setPressed(false);
            bm.setSelected(false);
            bm.setRollover(false);
            repaint();
        }
    }

    @Override
    public void mouseEntered (MouseEvent evt) {
    }

    @Override
    public void mouseExited (MouseEvent evt) {
    }
}
