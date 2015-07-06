/* -*- Mode: Java; tab-width: 4; indent-tabs-mode: nil; c-basic-offset: 4 -*- vim:set ts=4 sw=4 sts=4 et: */
package jp.oist.flint.form;

import jp.oist.flint.form.sub.SubFrame;
import jp.oist.flint.form.util.ComponentFactory;

import java.awt.Component;
import java.awt.Container;
import java.awt.Dimension;
import java.awt.FlowLayout;
import java.awt.GridLayout;
import java.awt.Point;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.awt.event.MouseEvent;
import java.awt.event.MouseListener;
import java.beans.PropertyChangeListener;
import java.io.File;
import java.util.ArrayList;
import java.util.HashMap;
import javax.swing.Box;
import javax.swing.BoxLayout;
import javax.swing.ButtonModel;
import javax.swing.DefaultListModel;
import javax.swing.DefaultListSelectionModel;
import javax.swing.ImageIcon;
import javax.swing.JButton;
import javax.swing.JList;
import javax.swing.JPanel;
import javax.swing.JProgressBar;
import javax.swing.JScrollPane;
import javax.swing.ListCellRenderer;
import javax.swing.ListModel;
import javax.swing.ListSelectionModel;
import javax.swing.border.EtchedBorder;
import javax.swing.border.TitledBorder;
import javax.swing.event.ListDataEvent;
import javax.swing.event.ListDataListener;

public class ProgressPane extends PeripheralPane
    implements MainFrame.Listener {

    public final static String TITLE = "Progresses";

    public final static String STOP_ACTION_COMMAND      = "progress.stop";

    public final static String CANCEL_ACTION_COMMAND    = "progress.cancel";

    public final static String LOG_ACTION_COMMAND       = "progress.log";

    public final static String JOB_ACTION_COMMAND      = "progress.job";

    private final static ProgressPane mInstance = new ProgressPane();

    public static ProgressPane getInstance () {
        return mInstance;
    }

    private ProgressPane.List mProgressList;

    private ProgressPane () {
        super(TITLE);
        initComponents();
    }

    protected Container createContentPane () {
        mProgressList = new ProgressPane.List();
        JScrollPane scrollPane = new JScrollPane(mProgressList);

        return scrollPane;
    }

    private void initComponents () {
        setContentPane(createContentPane());
    }

    public void addRow (ProgressPane.ListCell row) {
        DefaultListModel model = (DefaultListModel)mProgressList.getModel();
        model.addElement(row);
    }

    public ProgressPane.ListCell createListCell(SubFrame container) {
        String title = container.getModelFile().getName();
        ProgressPane.ListCell retval =  new ListCell(mProgressList, title);
        retval.setToolTipText(container.getModelFile().getPath());
        retval.setValue("container", container);
        return retval;
    }

    public int getSelectedIndex () {
        return mProgressList.getSelectedIndex();
    }

    public ProgressPane.ListCell getSelectedItem () {
        return (ProgressPane.ListCell)mProgressList.getSelectedValue();
    }

    public void setSelectedIndex (int index) {
        mProgressList.setSelectedIndex(index);
    }

    public void setSelectedCell (ProgressPane.ListCell plcp, boolean selected) {
        mProgressList.setSelectedValue(plcp, selected);
    }

    public ProgressPane.ListCell[]  getListCells () {
        DefaultListModel model = (DefaultListModel)mProgressList.getModel();
        int size = model.getSize();
        ProgressPane.ListCell[] retvals = new ProgressPane.ListCell[size];
        for (int i=0; i<size; i++) 
            retvals[i] = (ProgressPane.ListCell)model.getElementAt(i);

        return  retvals;
    }

    public ProgressPane.ListCell getListCell (int index) {
        DefaultListModel model = (DefaultListModel)mProgressList.getModel();
        return (ProgressPane.ListCell)model.getElementAt(index);
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

    public void removeListCell (ProgressPane.ListCell cell) {
        DefaultListModel model = (DefaultListModel)mProgressList.getModel();
        model.removeElement(cell);
    }

    public ProgressPane.ListCell getListCellOfModel (File modelFile) {
        DefaultListModel model = (DefaultListModel)mProgressList.getModel();
        int size = model.getSize();

        for (int i=0; i<size; i++) {
            ProgressPane.ListCell cell = (ProgressPane.ListCell)model.getElementAt(i);
            SubFrame container = (SubFrame)cell.getValue("container");
            if (container.getModelFile().equals(modelFile))
                return cell;
        }

        return null;
    }

    /*
     * Implements MainFrame.Listener 
     */
    @Override
    public void onModelOpened(MainFrame.Event evt) {
        final SubFrame subFrame = evt.getTarget();
        File file = subFrame.getModelFile();
        ProgressPane.ListCell plcp = createListCell(subFrame);
        addRow(plcp);
        subFrame.setStatusComponent(plcp);
        plcp.setToolTipText(String.format("%s [%s]", file.getName(), file.getPath()));
    }

    @Override
    public void onModelClosed(MainFrame.Event evt) {
        final SubFrame subFrame = evt.getTarget();

        if (subFrame == null)
            return;

        ProgressPane.ListCell[] cells = getListCells();
        for (ProgressPane.ListCell cell : cells) {
            if (subFrame.equals(cell.getValue("container"))) {
                removeListCell(cell);
                break;
            }
        }
    }

    /**
     * Child Components
     */
    private class List extends JList 
        implements ListDataListener, MouseListener, ListCellRenderer {

        private JButton mPressedBtn;

        public List () {
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

            ProgressPane.ListCell cellPane = 
                 (ProgressPane.ListCell)getModel().getElementAt(index);

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
            ProgressPane.ListCell cellPane = (ProgressPane.ListCell)value;

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

    public static class ListCell extends JPanel
            implements ActionListener {

        public final static String IS_SELECTED_PROPERTY = "selected";

        private final JList mParent;

        private JButton mJobBtn = null;

        private JButton mLogBtn = null;

        private JButton mCancelBtn = null;

        private JButton mStopBtn = null;

        private JProgressBar mProgressBar = null;

        private boolean mIsSelected = false;

        private final HashMap<String, Object> mVars = 
                            new HashMap<>();

        private final ArrayList<ActionListener> mActionListeners;

        private final ArrayList<PropertyChangeListener> mPropertyChangeListeners;

        public ListCell(JList parent, String title) {
            mParent = parent;
            mPropertyChangeListeners = new ArrayList<>();
            mActionListeners = new ArrayList<>();
            initComponents(title);

        }

        private void initComponents(String title) {
            setLayout(new GridLayout(2,1));
            setOpaque(true);
            setBorder(new TitledBorder(new EtchedBorder(), title));

            setPreferredSize(new Dimension(140, 75));

            mProgressBar = new JProgressBar();
            mJobBtn = ComponentFactory.createSquareButton("Detail", "progress.plot", new Dimension(48,20));
            mJobBtn.setActionCommand(JOB_ACTION_COMMAND);
            mJobBtn.addActionListener(this);
            mJobBtn.putClientProperty("owner", this);

            mLogBtn = ComponentFactory
                    .createSquareButton("Log", "progress.details", new Dimension(48,20));
            mLogBtn.setActionCommand(LOG_ACTION_COMMAND);
            mLogBtn.addActionListener(this);
            mLogBtn.putClientProperty("owner", this);
            mLogBtn.setVisible(false); // TODO 

            JPanel upperPane = new JPanel();
            upperPane.setLayout(new BoxLayout(upperPane, BoxLayout.LINE_AXIS));
            upperPane.add(Box.createRigidArea(new Dimension(5,0)));

            JPanel bottomPane = new JPanel(new FlowLayout(FlowLayout.RIGHT));

            bottomPane.setPreferredSize(new Dimension(10,10));
            ((FlowLayout)bottomPane.getLayout()).setVgap(0);
            ((FlowLayout)bottomPane.getLayout()).setHgap(0);

            upperPane.setOpaque(false);
            bottomPane.setOpaque(false);

            // progress bar
            Dimension progressSize = new Dimension(new Dimension(80, 20));
            mProgressBar.setMaximumSize(new Dimension(Short.MAX_VALUE, 20));
            mProgressBar.setMinimumSize(progressSize);
            mProgressBar.setPreferredSize(progressSize);
            mProgressBar.setSize(progressSize);
            mProgressBar.setBorderPainted(true);
            mProgressBar.setStringPainted(true);
            mProgressBar.setString("idle");

            int spaceW = 1; int spaceH = 20;

            upperPane.add(createSpacePanel(new Dimension(spaceW, spaceH)));
            upperPane.add(mProgressBar);
            upperPane.add(createSpacePanel(new Dimension(spaceW, spaceH)));

            mCancelBtn  = new JButton();
            mCancelBtn.setActionCommand(CANCEL_ACTION_COMMAND);
            mCancelBtn.addActionListener(this);
            mCancelBtn.setIcon(new ImageIcon(getClass().getResource("/jp/oist/flint/image/cancel.png")));
            Dimension btnSize = new Dimension(20,20);
            mCancelBtn.setSize(btnSize);
            mCancelBtn.setPreferredSize(btnSize);
            mCancelBtn.setMaximumSize(btnSize);
            mCancelBtn.setMinimumSize(btnSize);
            mCancelBtn.setOpaque(false);
            mCancelBtn.setEnabled(false);
            mCancelBtn.putClientProperty("owner", this);
            upperPane.add(mCancelBtn);

            mStopBtn  = new JButton();
            mStopBtn.setActionCommand(STOP_ACTION_COMMAND);
            mStopBtn.addActionListener(this);
            mStopBtn.setIcon(new ImageIcon(getClass().getResource("/jp/oist/flint/image/stop.png")));
            mStopBtn.setSize(btnSize);
            mStopBtn.setPreferredSize(btnSize);
            mStopBtn.setMaximumSize(btnSize);
            mStopBtn.setMinimumSize(btnSize);
            mStopBtn.setOpaque(false);
            mStopBtn.setEnabled(false);
            mStopBtn.putClientProperty("owner", this);

            upperPane.add(createSpacePanel(new Dimension(spaceW, spaceH)));

            add(upperPane);

            FlowLayout buttonLayout = new FlowLayout(FlowLayout.RIGHT);
            JPanel buttonPane = new JPanel(buttonLayout);
            buttonPane.setOpaque(false);
            buttonLayout.setHgap(1);
            buttonLayout.setVgap(3);
            buttonPane.add(mJobBtn);
            buttonPane.add(mLogBtn);

            // plot button
            bottomPane.setPreferredSize(new Dimension(120, 25));
            bottomPane.add(buttonPane);

            add(bottomPane);
        }

        private JPanel createSpacePanel (Dimension fixedSize) {
            JPanel panel = new JPanel();
            panel.setSize(fixedSize);
            panel.setPreferredSize(fixedSize);
            panel.setMaximumSize(fixedSize);
            panel.setMinimumSize(fixedSize);

            panel.setRequestFocusEnabled(false);
            panel.setOpaque(false);

            return panel; 
        }

        public void setGeneralButtonEnabled (boolean b) {
            mJobBtn.setEnabled(b);
            mLogBtn.setEnabled(b);
            repaint();
        }

        public synchronized void setText (String txt) {
            TitledBorder titledBorder = (TitledBorder)getBorder();
            titledBorder.setTitle(txt);
        }

        public synchronized String getText () {
            TitledBorder titledBorder = (TitledBorder)getBorder();
            return titledBorder.getTitle();
        }

        public void setSelected (boolean isSelected) {
            if (mIsSelected == isSelected)
                return;

            Boolean oldValue = mIsSelected;
            Boolean newValue = isSelected;
            firePropertyChange(IS_SELECTED_PROPERTY, oldValue, newValue);

            mIsSelected = isSelected;

            repaint();
        }

        public boolean isSelected () {
            return mIsSelected;
        }

        public int getStatusBarProgress () {
            return mProgressBar.getValue();
        }

        public void setProgress(String msg, int minimum, int maximum, int value) {
            int oldValue = mProgressBar.getValue();
            mProgressBar.setString(msg);
            mProgressBar.setValue(value);

            DefaultListModel model = (DefaultListModel)mParent.getModel();
            int myIndex = model.indexOf(this);

            // trigger to repaint the ListCell
            mParent.repaint(mParent.getCellBounds(myIndex, myIndex));
        }

        public void progressStarted () {
            String msg = "preparing...";

            mProgressBar.setValue(0);
            mProgressBar.setString(msg);
            setGeneralButtonEnabled(true);
            mCancelBtn.setEnabled(true);
        }

        public void progressFinished(String msg, int minimum, int maximum, int value) {
            setProgress(msg, minimum, maximum, value);
            mCancelBtn.setEnabled(false);
        }

        public void setValue (String key, Object var) {
            mVars.put(key, var);
        }

        public Object getValue (String key) {
            return mVars.get(key);
        }

        public void addActionListener (ActionListener l) {
            mActionListeners.add(l);
        }

        public void removeActionListener (ActionListener l) {
            mActionListeners.remove(l);
        }

        @Override
        public void actionPerformed(ActionEvent evt) {
            fireActionPerformed(evt);
        }

        protected void fireActionPerformed (ActionEvent evt) {
            for (ActionListener l : mActionListeners)
                l.actionPerformed(evt);
        }
    }
}
