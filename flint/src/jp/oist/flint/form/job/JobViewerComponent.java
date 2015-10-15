/* -*- Mode: Java; tab-width: 4; indent-tabs-mode: nil; c-basic-offset: 4 -*- vim:set ts=4 sw=4 sts=4 et: */
package jp.oist.flint.form.job;

import jp.oist.flint.job.Progress;
import java.awt.AWTEvent;
import java.awt.Color;
import java.awt.Component;
import java.awt.Dimension;
import java.awt.LayoutManager;
import java.awt.Point;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.awt.event.MouseEvent;
import java.awt.event.MouseListener;
import java.beans.PropertyChangeEvent;
import java.beans.PropertyChangeListener;
import java.util.EventObject;
import java.util.HashMap;
import java.util.Map;
import javax.swing.DefaultListSelectionModel;
import javax.swing.JMenuItem;
import javax.swing.JPanel;
import javax.swing.JPopupMenu;
import javax.swing.ListModel;
import javax.swing.ListSelectionModel;
import javax.swing.SwingUtilities;
import javax.swing.UIManager;
import javax.swing.event.ListDataEvent;
import javax.swing.event.ListDataListener;
import javax.swing.event.ListSelectionEvent;
import javax.swing.event.ListSelectionListener;

public abstract class JobViewerComponent extends JPanel
    implements ListDataListener, ListSelectionListener, 
               MouseListener {

    public final static JobViewerComponent EMPTY = new JobViewerComponent.Empty();

    private ContextMenu mContextMenu;

    private IParameterInfo mParameterInfo;

    private ListModel mDataModel;

    private ListSelectionModel mSelectionModel;

    private Color mSelectionForeground;

    private Color mSelectionBackground;

    private JobViewerComponent.ContextMenuHandler mContextMenuHandler;

    public static JobViewerComponent factory (IParameterInfo pInfo) {
        JobViewerComponent viewer = EMPTY;

        if (ScatterChart.canPlot (pInfo)) {
            viewer = new ScatterChart(pInfo);
        } else if (RadarChartList.canPlot(pInfo)) {
            viewer = new RadarChartList(pInfo);
        } 

        return viewer;
    }

    public JobViewerComponent (LayoutManager layout) {
        this(layout, null);
    }

    protected JobViewerComponent (LayoutManager layout, IParameterInfo parameterInfo) {
        super(layout);

        mContextMenuHandler = new JobViewerComponent.ContextMenuHandler() {
            @Override
            public void handleEvent(Event evt) {
                // empty method.
            }
        };
        mParameterInfo = parameterInfo;
        mContextMenu = new ContextMenu();
        initComponents();
    }

    private void initComponents () {
        setOpaque(false);
        setModel(new CombinationModel());
        setSelectionModel(new DefaultListSelectionModel());

        setForeground(UIManager.getColor("List.foreground")); 
        setBackground(UIManager.getColor("List.background"));

        setFocusable(true);

        addMouseListener(this);
    }

    public void setParameterInfo (IParameterInfo pInfo) {
        IParameterInfo old = mParameterInfo;
        mParameterInfo = pInfo;

        firePropertyChange("parameterInfo", old, mParameterInfo);
    }

    public int getSelectedIndex () {
        return getSelectionModel().getMinSelectionIndex();
    }

    public int[] getSelectedIndices () {
        int min = getSelectionModel().getMinSelectionIndex();
        int max = getSelectionModel().getMaxSelectionIndex();

        if (min <0 || max <0)
            return new int[0];

        int[] tmp = new int[max - min + 1];
        int n = 0;
        for (int i=min; i<=max; i++) {
            if (getSelectionModel().isSelectedIndex(i))
                tmp[n++] = i;
        }

        int[] retval = new int[n];
        System.arraycopy(tmp, 0, retval, 0, n);
        return retval;
    }

    public IParameterInfo getParameterInfo () {
        return mParameterInfo;
    }

    public ListModel getModel () {
        return mDataModel;
    }

    public void setModel (ListModel model) {
        if (model == null)
            throw new IllegalArgumentException("model must not be null");

        ListModel old = mDataModel;
        mDataModel = model;
        firePropertyChange("model", old, mDataModel);
    }

    public ListSelectionModel getSelectionModel() {
        return mSelectionModel;
    }

    public void setSelectionModel (ListSelectionModel selectionModel) {
        if (selectionModel == null)
            throw new IllegalArgumentException("selectionModel must not be null");

        ListSelectionModel old = mSelectionModel;
        mSelectionModel = selectionModel;
        firePropertyChange("selectionModel", old, mSelectionModel);
    }

    public int getActiveParameterCount() {
        if (mParameterInfo != null)
            return mParameterInfo.getActiveParameterCount();
        return -1;
    }

    public int[] getActiveIndices () {
        if (mParameterInfo != null)
            return mParameterInfo.getActiveIndexes();
        return null;
    }

    public void setSelectionForeground (Color c) {
        Color old = mSelectionForeground;
        mSelectionForeground = c;
        firePropertyChange("selectionForeground", old, mSelectionForeground);
    }

    public Color getSelectionForeground () {
        return mSelectionForeground;
    }

    public void setSelectionBackground (Color c) {
        Color old = mSelectionBackground;
        mSelectionBackground = c;

        firePropertyChange("selectionBackground", old, mSelectionBackground);
    }

    public Color getSelectionBackground () {
        return mSelectionBackground;
    }

    public Number[] getValues (int index) {
        return (Number[])mDataModel.getElementAt(index);
    }

    public Number getValue (int index, int valueIndex) {
        return ((Number[])mDataModel.getElementAt(index))[valueIndex];
    }

    public void setContextMenuHandler (JobViewerComponent.ContextMenuHandler handler) {
        JobViewerComponent.ContextMenuHandler old = mContextMenuHandler;

        mContextMenuHandler = handler;
        firePropertyChange("handler", old, mContextMenuHandler);
    }

    public void handleContextMenuEvent (JobViewerComponent.Event evt) {
        if (mContextMenuHandler != null)
            mContextMenuHandler.handleEvent(evt);
    }

    protected void firePropertyChange(PropertyChangeEvent evt) {
        for (PropertyChangeListener l : getPropertyChangeListeners())
            l.propertyChange(evt);
    }

    @Override
    public void firePropertyChange (String propertyName, Object oldValue, Object newValue) {
        firePropertyChange(new PropertyChangeEvent(this, propertyName, oldValue, newValue));
    }

    protected void dispatchEventToParent (AWTEvent evt) {
        if (getParent() != null)
            getParent().dispatchEvent(evt);
    }

    protected MouseEvent transformPoint(MouseEvent evt) {
        return SwingUtilities.convertMouseEvent((Component)evt.getSource(), evt, this);
    }

    @Override
    public void intervalAdded(ListDataEvent lde) {
    }

    @Override
    public void intervalRemoved(ListDataEvent lde) {
    }

    @Override
    public void contentsChanged(ListDataEvent lde) {
    }

    @Override
    public void valueChanged(ListSelectionEvent lse) {
    }

    abstract public boolean getValueIsAdjusting (int index);

    abstract public void setValueIsAdjusting(int index, boolean isAdjusting);

    abstract public int getProgress (int index);

    abstract public void setProgress(int index, Progress progress);

    abstract public boolean isCancelled (int index);

    abstract public void setCancelled (int index, boolean cancelled);

    abstract public void ensureIndexIsVisible (int index);

    abstract public int locationToIndex (Point p);

    abstract public Map<Integer, Number> getValuesAtHover (Point p);

    abstract public Map<Integer, Number> getValuesAt (Point p);

    @Override
    public void mouseClicked(MouseEvent evt) {
        if (mContextMenu.isVisible())
            mContextMenu.setVisible(false);

        if (SwingUtilities.isRightMouseButton(evt) 
                && evt.getClickCount() == 1
                && locationToIndex(evt.getPoint()) != -1) {
            mContextMenu.show(JobViewerComponent.this, evt.getX(), evt.getY());
        }


        int index = locationToIndex(evt.getPoint());
        if (evt.getClickCount() == 2 && index >= 0) {
            JobViewerComponent.Event e = new JobViewerComponent.Event(this, "view", index);
            mContextMenuHandler.handleEvent(e);
        }
    }

    @Override
    public void mousePressed(MouseEvent evt) {
    }

    @Override
    public void mouseReleased(MouseEvent evt) {
    }

    @Override
    public void mouseEntered(MouseEvent evt) {
    }

    @Override
    public void mouseExited(MouseEvent evt) {
    }

    public static class Event extends EventObject {

        Map <String, Object> mClientProperty;

        String mAction;

        int mIndex;

        public Event(Object source, String action, int index) {
            super(source);

            mAction = action;
            mIndex = index;

            mClientProperty = new HashMap<>();
        }

        public void putClientProperty (String key, Object value) {
            mClientProperty.put(key, value);
        }

        public Object getClientProperty (String key) {
            Object value;
            value = (mClientProperty.containsKey(key))?
                mClientProperty.get(key) : null;
            return value;
        }

        public String getAction () {
            return mAction;
        }

        public int getIndex () {
            return mIndex;
        }

    }

    public static class Empty extends JobViewerComponent {

        private Empty () {
            super(null);
        }

        @Override
        public boolean getValueIsAdjusting(int index) {
            return false;
        }

        @Override
        public void setValueIsAdjusting(int index, boolean isAdjusting) {
            // nothing to do...
        }

        @Override
        public int getProgress(int index) {
            return 0;
        }

        @Override
        public void setProgress(int index, Progress progress) {
            // nothing to do...
        }

        @Override
        public boolean isCancelled(int index) {
            return false;
        }

        @Override
        public void setCancelled(int index, boolean cancelled) {
            // nothing to do...
        }

        @Override
        public void ensureIndexIsVisible(int index) {
            // nothing to do...
        }

        @Override
        public int locationToIndex(Point p) {
            return -1;
        }

        @Override
        public Map<Integer, Number> getValuesAtHover(Point p) {
            return null;
        }

        @Override
        public Map<Integer, Number> getValuesAt(Point p) {
            return null;
        }
    }

    public static abstract class ContextMenuHandler {
        abstract public void handleEvent(Event evt);
    }

    public class ContextMenu extends JPopupMenu 
        implements ActionListener {

        public final static String ACTION_VIEW = "jobviewer.contextmenu.action.view";
        public final static String ACTION_EXPORT = "jobviewer.contextmenu.action.export";
        public final static String ACTION_SENDVIAGARUDA = "jobviewer.contextmenu.action.sendviagaruda";
        public final static String ACTION_CANCEL = "jobviewer.contextmenu.action.cancel";

        private JMenuItem btn_View;
        private JMenuItem btn_Export;
        private JMenuItem btn_SendViaGaruda;
        private JMenuItem btn_Cancel;

        public ContextMenu () {
            super();

            initComponents();
        }

        private void initComponents () {
            btn_View        = new JMenuItem("View");
            btn_Export      = new JMenuItem("Export");
            btn_SendViaGaruda = new JMenuItem("Send via Garuda");
            btn_Cancel      = new JMenuItem("Cancel");

            Dimension dim = new Dimension(200, 80);

            setPreferredSize(dim);
            setSize(dim);

            add(btn_View);
            btn_View.addActionListener(this);
            btn_View.setActionCommand(ACTION_VIEW);

            add(btn_Export);
            btn_Export.addActionListener(this);
            btn_Export.setActionCommand(ACTION_EXPORT);

            add(btn_SendViaGaruda);
            btn_SendViaGaruda.addActionListener(this);
            btn_SendViaGaruda.setActionCommand(ACTION_SENDVIAGARUDA);

            addSeparator();

            add(btn_Cancel);
            btn_Cancel.addActionListener(this);
            btn_Cancel.setActionCommand(ACTION_CANCEL);
        }

        @Override
        public void actionPerformed(ActionEvent evt) {
            String actionCommand = evt.getActionCommand();
            int selectedIndex = getSelectedIndex();
            if (ACTION_VIEW.equals(actionCommand)) {
                handleContextMenuEvent(new JobViewerComponent.Event(JobViewerComponent.this, 
                        "view", selectedIndex));
            } else if (ACTION_EXPORT.equals(actionCommand)) {
                handleContextMenuEvent(new JobViewerComponent.Event(JobViewerComponent.this, 
                        "export", selectedIndex));
            } else if (ACTION_SENDVIAGARUDA.equals(actionCommand)) {
                handleContextMenuEvent(new JobViewerComponent.Event(JobViewerComponent.this, 
                        "sendViaGaruda", selectedIndex));
            } else if (ACTION_CANCEL.equals(actionCommand)) {
                handleContextMenuEvent(new JobViewerComponent.Event(JobViewerComponent.this, 
                        "cancelJob", selectedIndex));
            }
        }
    }
}