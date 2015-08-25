/* -*- Mode: Java; tab-width: 4; indent-tabs-mode: nil; c-basic-offset: 4 -*- vim:set ts=4 sw=4 sts=4 et: */
package jp.oist.flint.form;

import java.awt.Component;
import java.awt.Container;
import java.awt.Point;
import java.awt.event.MouseEvent;
import java.awt.event.MouseListener;
import javax.swing.ButtonModel;
import javax.swing.DefaultListModel;
import javax.swing.DefaultListSelectionModel;
import javax.swing.JButton;
import javax.swing.JList;
import javax.swing.ListCellRenderer;
import javax.swing.ListModel;
import javax.swing.ListSelectionModel;
import javax.swing.event.ListDataEvent;
import javax.swing.event.ListDataListener;

class ProgressList extends JList
    implements ListDataListener, MouseListener, ListCellRenderer {

    private JButton mPressedBtn;

    public ProgressList () {
        super();
        super.setModel(createListDataModel());
        super.setSelectionModel(createListSelectionModel());
        initComponents();
    }

    private ListModel createListDataModel () {
        DefaultListModel model = new DefaultListModel();
        model.addListDataListener(this);
        return model;
    }

    private ListSelectionModel createListSelectionModel () {
        ListSelectionModel model = new DefaultListSelectionModel();
        model.setSelectionMode(ListSelectionModel.SINGLE_SELECTION);

        return model;
    }


    private void initComponents () {
        super.setSelectionMode(ListSelectionModel.SINGLE_SELECTION);
        super.setDragEnabled(false);

        setCellRenderer(this);
        addMouseListener(this);
        setOpaque(true);
    }

    private JButton getButtonAt(Point point) {
        int index = locationToIndex(point);
        if (index == -1) return null;

        ProgressCell cellPane =
            (ProgressCell)getModel().getElementAt(index);

        Point location = indexToLocation(index);
        cellPane.setLocation(location);
        Component c = findComponentAt(cellPane, point);
        if (c instanceof JButton) {
            JButton btn = (JButton)c;
            return btn;
        }
        return null;
    }

    private Component findComponentAt (Component c, Point point) {
        Point location = c.getLocation();
        Point rpoint = new Point(
                                 point.x - location.x,
                                 point.y - location.y
                                 );
        if (c instanceof Container) {
            Component[] children  = ((Container)c).getComponents();
            for (Component child : children) {
                if (child.getBounds().contains(rpoint)){
                    if (child instanceof Container) {
                        if (((Container)child).getComponents().length > 0) {
                            return findComponentAt(child, rpoint);
                        } else {
                            return child;
                        }
                    }
                }
            }
        }
        return null;
    }

    @Override
    public void setDragEnabled(boolean b) {
        String msg = "Cannnot use the drag/drop feature.";
        throw new UnsupportedOperationException(msg);
    }

    @Override
    public void setSelectionMode (int mode) {
        String msg = "SelectionMode be able to use only SingleSelection.";
        throw new UnsupportedOperationException(msg);
    }

    @Override
    public void setModel (ListModel model) {
        String msg = "Cannnot set the ListModel.";
        throw new UnsupportedOperationException(msg);
    }

    @Override
    public void setSelectionModel (ListSelectionModel model) {
        String msg = "Cannnot set the ListSelectionModel.";
        throw new UnsupportedOperationException(msg);
    }

    @Override
    public void intervalAdded(ListDataEvent evt) { }

    @Override
    public void contentsChanged(ListDataEvent e) { }

    @Override
    public void intervalRemoved(ListDataEvent evt) {
        int deletedIndex = evt.getIndex0();
        int selectedIndex = deletedIndex-1;
        if(selectedIndex < 1) selectedIndex = 0;
        setSelectedIndex(selectedIndex);
    }

    @Override
    public Component getListCellRendererComponent(JList list, Object value,
                                                  int index, boolean isSelected, boolean hasFocus) {
        ProgressCell cellPane = (ProgressCell)value;

        cellPane.setEnabled(list.isEnabled());
        cellPane.setFont(list.getFont());

        if (isSelected) {
            cellPane.setBackground(list.getSelectionBackground());
            cellPane.setForeground(list.getSelectionForeground());
        } else {
            cellPane.setBackground(list.getBackground());
            cellPane.setForeground(list.getForeground());
        }

        cellPane.setOpaque(true);
        cellPane.setSelected(isSelected);
        return cellPane;
    }

    @Override
    public void mouseClicked(MouseEvent e) {
        JButton btn = getButtonAt(e.getPoint());
        if (btn != null) btn.doClick();
    }

    @Override
    public void mousePressed(MouseEvent e) {
        mPressedBtn = getButtonAt(e.getPoint());
        if (mPressedBtn != null) {
            ButtonModel bm = mPressedBtn.getModel();
            bm.setArmed(true);
            bm.setPressed(true);
            bm.setSelected(true);
            bm.setRollover(true);
            repaint();
        }
    }

    @Override
    public void mouseReleased(MouseEvent e) {
        if (mPressedBtn != null) {
            ButtonModel bm = mPressedBtn.getModel();
            bm.setArmed(false);
            bm.setPressed(false);
            bm.setSelected(false);
            bm.setRollover(false);
            repaint();
        }
    }

    @Override
    public void mouseEntered(MouseEvent e) { }

    @Override
    public void mouseExited(MouseEvent e) { }
}
