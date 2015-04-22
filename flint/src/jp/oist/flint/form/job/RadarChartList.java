/* -*- Mode: Java; tab-width: 4; indent-tabs-mode: nil; c-basic-offset: 4 -*- vim:set ts=4 sw=4 sts=4 et: */
package jp.oist.flint.form.job;

import java.awt.Component;
import java.awt.Dimension;
import java.awt.Point;
import java.awt.Rectangle;
import java.awt.event.ActionEvent;
import java.awt.event.KeyEvent;
import java.awt.event.MouseEvent;
import java.awt.event.MouseListener;
import java.awt.event.MouseMotionListener;
import java.beans.PropertyChangeEvent;
import java.beans.PropertyChangeListener;
import java.util.HashMap;
import java.util.Map;
import javax.swing.AbstractAction;
import javax.swing.ActionMap;
import javax.swing.InputMap;
import javax.swing.JScrollPane;
import javax.swing.KeyStroke;
import javax.swing.ListModel;
import javax.swing.ListSelectionModel;
import javax.swing.UIManager;
import javax.swing.event.ListDataEvent;
import javax.swing.event.ListSelectionEvent;
import jp.oist.flint.form.layout.VerticalFlowLayout;

public class RadarChartList extends JobViewerComponent 
    implements PropertyChangeListener {

    public static boolean canPlot (IParameterInfo pInfo) {
        if (pInfo == null)
            return false;

        int activeParameterCount = pInfo.getActiveParameterCount();
        return activeParameterCount > 2;
    }

    private int[] mIgnoreIdexes;

    private int mAnchorIndex = -1;

    private final Dimension mCellSize = new Dimension(64, 64);

    private boolean isMacOS = false;

    private final int mMinimum = 0;

    private final int mMaximum = 100;

    private final RadarChartEventHandler mRadarChartEventHandler;

    public RadarChartList (IParameterInfo pInfo) {
        super(new VerticalFlowLayout(), pInfo);

        String osName = System.getProperty("os.name");
        if (osName.startsWith("Mac OS X"))
            isMacOS = true;

        mRadarChartEventHandler = new RadarChartEventHandler();

        initComponents();
        initEvents();
        setFocusable(true);
    }

    private void initComponents () {
        setOpaque(true);
        setForeground(UIManager.getColor("List.foreground"));
        setBackground(UIManager.getColor("List.background"));
        setSelectionForeground(UIManager.getColor("List.selectionForeground"));
        setSelectionBackground(UIManager.getColor("List.selectionBackground"));

        InputMap inMap =  getInputMap(RadarChartList.WHEN_FOCUSED);
        ActionMap acMap = getActionMap();

        String key = "clearSelection";
        inMap.put(KeyStroke.getKeyStroke(KeyEvent.VK_ESCAPE, 0), key);
        acMap.put(key, new ClearSelectionAction());

        key = "selectUp";
        inMap.put(KeyStroke.getKeyStroke(KeyEvent.VK_UP, 0), key);
        acMap.put(key, new MoveSelectionAction(key));

        key = "selectDown";
        inMap.put(KeyStroke.getKeyStroke(KeyEvent.VK_DOWN, 0), key); acMap.put(key, new MoveSelectionAction(key));

        key = "selectLeft";
        inMap.put(KeyStroke.getKeyStroke(KeyEvent.VK_LEFT, 0), key);
        acMap.put(key, new MoveSelectionAction(key));

        key = "selectRight";
        inMap.put(KeyStroke.getKeyStroke(KeyEvent.VK_RIGHT, 0), key);
        acMap.put(key, new MoveSelectionAction(key));
    }

    private void initEvents () {
        addPropertyChangeListener(this);
        getModel().addListDataListener(this);
        getSelectionModel().addListSelectionListener(this);
    }

    private JScrollPane getJScrollPane () {

        Component c = this;
        while (c != null) {
            c = c.getParent();

            if (c instanceof JScrollPane)
                return (JScrollPane)c;
        }
        return null;
    }

    private void moveScrollBar (int index) {
        JScrollPane scrollPane = getJScrollPane();

        if (scrollPane == null) 
            return;

        int columnCount = getSize().width / mCellSize.width;
        int row = (int)Math.floor(index / columnCount);
        int movement = row*mCellSize.height;
        scrollPane.getVerticalScrollBar().setValue(movement);
    }

    public boolean isSelectedIndex (int index) {
        return getSelectionModel().isSelectedIndex(index);
    }

    public int indexOf (Component c) {
        for (int i=0; i<getComponentCount(); i++) {
            if (getComponent(i).equals(c))
                return i;
        }
        return -1;
    }

    @Override
    public int locationToIndex (Point p) {
        for (int i=0; i<getComponentCount(); i++) {
            Component c = getComponent(i);
            Rectangle bounds = c.getBounds();
            if (bounds.x <= p.x && p.x <= (bounds.x+bounds.width)
                && bounds.y <= p.y && p.y <= (bounds.y+bounds.height))
                return i;
        }
        return -1;
    }

    private int vertexToIndex (int vertexIndex) {
        int[] activeIndecies = getParameterInfo().getActiveIndexes();
        if (activeIndecies == null)
            return -1;
        return activeIndecies[vertexIndex];
    }

    public Map<Integer, Number> getValuesAt (Point p, boolean isHoverPoint) {
        int index = locationToIndex(p);
        if (index < 0)
            return null;

        HashMap<Integer, Number> ret = new HashMap<>();
        try {
            RadarChartProgress child = (RadarChartProgress)getComponent(index);
            Point newPoint = (Point)p.clone();
            newPoint.translate(-child.getBounds().x, -child.getBounds().y);
            int vertexIndex = child.backgroundVertexAtPoint(newPoint);

            if (vertexIndex >= 0 && isHoverPoint) {
                ret.put(vertexToIndex(vertexIndex), child.getValue(vertexIndex));
                return ret;
            } else {
                IParameterInfo pInfo = getParameterInfo();
                Number[] values = ((CombinationModel)getModel()).getValues(index);

                int count = pInfo.getParameterCount();
                for (int i=0; i<count; i++)
                    ret.put(i, values[i]);
                return ret;
            }
        } catch (ClassCastException ex) { }

        return null;

    }

    @Override
    public Map<Integer, Number> getValuesAt (Point p) {
        return getValuesAt(p, false);
    }

    @Override
    public Map<Integer, Number> getValuesAtHover (Point p) {
        return getValuesAt(p, true);
    }

    @Override
    public boolean getValueIsAdjusting (int index) {
        return ((RadarChartProgress)getComponent(index)).getValueIsAdjusting();
    }

    @Override
    public void setValueIsAdjusting (int index, boolean isAdjusting) {
        ((RadarChartProgress)getComponent(index)).setValueIsAdjusting(isAdjusting);
    }

    @Override
    public int getProgress (int index) {
        if (index >= getComponentCount())
            return -1;
        return ((RadarChartProgress)getComponent(index)).getProgress();
    }

    @Override
    public void setProgress (int index, int progress) {
        if (index >= getComponentCount())
            return;

        RadarChartProgress rcp = (RadarChartProgress) getComponent(index);
        rcp.setProgress(progress);
        rcp.repaint();
    }

    @Override
    public boolean isCancelled (int index) {
        return ((RadarChartProgress)getComponent(index)).isCancelled();
    }

    @Override
    public void setCancelled (int index, boolean cancelled) {
        ((RadarChartProgress)getComponent(index)).setCancelled(cancelled);
    }

    @Override
    public void ensureIndexIsVisible (int index) {
        moveScrollBar(index);
    }

    @Override
    protected MouseEvent transformPoint(MouseEvent evt) {
        Component source = this;
        Component child = (Component)evt.getSource();

        Point p = evt.getPoint();
        p.translate(child.getBounds().x, child.getBounds().y);

        return new MouseEvent (
                source, evt.getID(), evt.getWhen(), evt.getModifiers(),
                p.x, p.y, evt.getClickCount(), evt.isPopupTrigger(), 
                evt.getButton());
    }


    @Override
    public void intervalAdded (ListDataEvent evt) {
        ListModel model = (ListModel)evt.getSource();
        int anchor = evt.getIndex0();
        int lead = evt.getIndex1();

        if (anchor < 0) return;

        Number[] maximums = null;
        Number[] minimums = null;
        String[] titles   = null;
        IParameterInfo pInfo = getParameterInfo();

        if (getParameterInfo() != null) {
            maximums = pInfo.getActiveMaximums();
            minimums = pInfo.getActiveMinimums();
            titles   = pInfo.getActiveTitles();
        }

        for (int i=anchor; i<=lead; i++) {
            Number[] row = (Number[])model.getElementAt(i);
            RadarChartProgress child = new RadarChartProgress(row.length);
            Dimension dim = new Dimension(mCellSize);
            child.setPreferredSize(dim);
            child.setSize(dim);
            child.setValues(row);

            child.setMaximumValues(maximums);
            child.setMinimumValues(minimums);
            child.setMaximumProgress(mMaximum);
            child.setMinimumProgress(mMinimum);
            child.setProgress(mMinimum);
            child.setValueIsAdjusting(false);
            child.setTitles(titles);
            child.setAntialiasing(true);
            child.addMouseListener(mRadarChartEventHandler);
            child.addMouseMotionListener(mRadarChartEventHandler);
            add(child, i);
        }
    }

    @Override
    public void intervalRemoved (ListDataEvent evt) {
        int anchor = evt.getIndex0();
        int lead = evt.getIndex1();

        for (int i=anchor; i<=lead; i++)
            remove(anchor);

    }

    @Override
    public void contentsChanged (ListDataEvent evt) {
        intervalRemoved(evt);

        intervalAdded(evt);
    }

    @Override
    public void valueChanged(ListSelectionEvent evt) {
       for (Component c : getComponents()) {
            c.setForeground(getForeground());
            c.setBackground(getBackground());
        }

        int[] selectedIndices = getSelectedIndices();
        for (int index : selectedIndices) {
            Component c = getComponent(index);
            c.setForeground(getSelectionForeground());
            c.setBackground(getSelectionBackground());
        }
    }

    @Override
    public void propertyChange(PropertyChangeEvent evt) {
        String propertyName = evt.getPropertyName();
        Object newValue = evt.getNewValue();
        Object oldValue = evt.getOldValue();

        if ("model".equals(propertyName)) {
            if (oldValue instanceof ListModel)
                ((ListModel)oldValue).removeListDataListener(this);

            if (newValue instanceof ListModel) {
                ((ListModel)newValue).addListDataListener(this);
                removeAll();
                if (getSelectionModel() != null)
                    getSelectionModel().clearSelection();
                ListModel model = getModel();

                ListDataEvent lde = new ListDataEvent(model,
                        ListDataEvent.INTERVAL_ADDED, 0, model.getSize()-1);
                intervalAdded(lde);
            }

        } else if ("selectionModel".equals(propertyName)) {
            if (oldValue instanceof ListSelectionModel)
                ((ListSelectionModel)oldValue).removeListSelectionListener(this);

            if (newValue instanceof ListSelectionModel)
                ((ListSelectionModel)newValue).addListSelectionListener(this);
        } else if ("background".equals(propertyName)) {
            for (Component c : getComponents())
                c.setBackground(getBackground());
        } else if ("foreground".equals(propertyName)) {
            for (Component c : getComponents())
                c.setForeground(getForeground());
        } else if ("maximum".equals(propertyName)) {
            for (Component c : getComponents()) {
                try {
                    ((RadarChartProgress)c).setMaximumProgress((Integer)newValue);
                } catch (ClassCastException ex) { }
            }
        } else if ("minimum".equals(propertyName)) {
            for (Component c : getComponents()) {
                try {
                    ((RadarChartProgress)c).setMinimumProgress((Integer)newValue);
                } catch (ClassCastException ex) { }
            }
        }
    }

    private class RadarChartEventHandler implements MouseListener, MouseMotionListener {
        @Override
        public void mouseClicked(MouseEvent evt) {
            Component source = evt.getComponent();
            Component dest   = RadarChartList.this;
//            dispatchEvent(SwingUtilities.convertMouseEvent(source, evt, dest));
        }

        @Override
        public void mousePressed(MouseEvent evt) {
            Object source = evt.getSource();
            if (source instanceof RadarChartProgress) {
                RadarChartProgress rcp = (RadarChartProgress)source;
                ListSelectionModel selectionModel = getSelectionModel();
                int index = indexOf(rcp);
                int anchor = mAnchorIndex;
                mAnchorIndex = index;

                selectionModel.setValueIsAdjusting(true);
                boolean isTypedShift = evt.isShiftDown();
                boolean isTypedCtrlOrMeta = (isMacOS)?
                        evt.isMetaDown() : evt.isControlDown();

                if (isTypedCtrlOrMeta) {
                    if (getSelectionModel().isSelectedIndex(index)) {
                        selectionModel.removeSelectionInterval(index, index);
                    } else {
                        selectionModel.addSelectionInterval(index, index);
                    }
                } else if (isTypedShift) {
                    if (selectionModel.isSelectedIndex(index)) {
                        selectionModel.removeSelectionInterval(anchor, index);
                    } else {
                        selectionModel.addSelectionInterval(anchor, index);
                    }
                } else {
                    selectionModel.setSelectionInterval(index, index);
                }
            }
            Component dest   = RadarChartList.this;
        }

        @Override
        public void mouseReleased(MouseEvent evt) {
            getSelectionModel().setValueIsAdjusting(false);
        }

        @Override
        public void mouseEntered(MouseEvent evt) {
            getSelectionModel().setValueIsAdjusting(false);
        }

        @Override
        public void mouseExited(MouseEvent evt) {
            getSelectionModel().setValueIsAdjusting(false);
        }

        @Override
        public void mouseDragged(MouseEvent evt) {
            getSelectionModel().setValueIsAdjusting(false);
        }

        @Override
        public void mouseMoved(MouseEvent evt) {
            getSelectionModel().setValueIsAdjusting(false);
        }
    }

    public class ClearSelectionAction extends AbstractAction {
        @Override
        public void actionPerformed(ActionEvent ae) {
            if (getSelectionModel() != null)
                getSelectionModel().clearSelection();
        }
    }
public class MoveSelectionAction extends AbstractAction {
        public MoveSelectionAction(String name) {
            super(name);
        }

        @Override
        public void actionPerformed(ActionEvent evt) {
            String name = (String)getValue(NAME);

            Dimension dim = getSize();
            int column = dim.width / mCellSize.width;
            int last = getModel().getSize()-1;
            int movement = 0;

            if (name.equals("selectUp")) {
                movement = mAnchorIndex - column;
                if (movement < 0) movement = mAnchorIndex;
            } else if (name.equals("selectDown")) {
                movement = mAnchorIndex + column;
                if (movement >= last) movement = last;
            } else if (name.equals("selectLeft")) {
                movement = mAnchorIndex-1;
                movement = (movement >= 0)? movement : 0;
            } else if (name.equals("selectRight")) {
                movement = mAnchorIndex+1;
                movement = (movement <= last)? movement : last;
            }
            getSelectionModel().setSelectionInterval(movement, movement);
            mAnchorIndex = movement;
            moveScrollBar(getSelectedIndex());
        }
    }
}