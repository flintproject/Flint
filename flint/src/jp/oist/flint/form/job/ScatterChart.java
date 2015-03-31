/* -*- Mode: Java; tab-width: 4; indent-tabs-mode: nil; c-basic-offset: 4 -*- vim:set ts=4 sw=4 sts=4 et: */
package jp.oist.flint.form.job;

import java.awt.BorderLayout;
import java.awt.Color;
import java.awt.Point;
import java.awt.event.MouseEvent;
import java.awt.event.MouseListener;
import java.awt.event.MouseMotionListener;
import java.beans.PropertyChangeEvent;
import java.beans.PropertyChangeListener;
import java.text.FieldPosition;
import java.text.NumberFormat;
import java.text.ParsePosition;
import java.util.HashMap;
import java.util.Map;
import javax.swing.ListModel;
import javax.swing.event.ListDataEvent;
import jp.oist.flint.view.FlintChartPanel;
import org.jfree.chart.ChartFactory;
import org.jfree.chart.ChartPanel;
import org.jfree.chart.ChartRenderingInfo;
import org.jfree.chart.JFreeChart;
import org.jfree.chart.axis.NumberAxis;
import org.jfree.chart.axis.NumberTickUnit;
import org.jfree.chart.axis.TickUnit;
import org.jfree.chart.axis.TickUnits;
import org.jfree.chart.entity.ChartEntity;
import org.jfree.chart.entity.XYItemEntity;
import org.jfree.chart.plot.PlotOrientation;
import org.jfree.chart.plot.XYPlot;
import org.jfree.data.xy.XYSeries;
import org.jfree.data.xy.XYSeriesCollection;

public class ScatterChart extends JobViewerComponent
                implements PropertyChangeListener {


    public static boolean canPlot (IParameterInfo pInfo) {
        if (pInfo == null)
            return false;

        int activeParameterCount = pInfo.getActiveParameterCount();
        return 0 < activeParameterCount && activeParameterCount <= 2;
    }

    private JFreeChart mChart;

    private ChartPanel mChartPanel;

    protected ScatterChart (IParameterInfo pInfo) {
        super(new BorderLayout(), pInfo);

        initComponents();

        initEvents();
    }

    private void initComponents () {
        XYSeriesCollection dataset = new XYSeriesCollection();
        XYSeries series = new XYProgressSeries("", false, true);
        dataset.addSeries(series);

        // createScatterPlot(chart-title, x-title, y-title, dataset);
        mChart = ChartFactory.createScatterPlot("", "", "", dataset, 
                            PlotOrientation.VERTICAL, false, true, true);
        XYPlot plot = (XYPlot)mChart.getPlot();
        mChart.setAntiAlias(true);

        plot.setBackgroundPaint(getBackground());
        plot.setRangeGridlinePaint(Color.gray);
        plot.setDomainGridlinePaint(Color.gray);
        plot.setOutlineVisible(true);

        XYProgressCircleRenderer renderer = new XYProgressCircleRenderer(this);
        plot.setRenderer(renderer);

        mChartPanel = new FlintChartPanel(mChart);
        mChartPanel.setRangeZoomable(true);

        ChartPanelEventHandler eventHandler = new ChartPanelEventHandler();

        mChartPanel.addMouseMotionListener(eventHandler);
        mChartPanel.addMouseListener(eventHandler);

        add(mChartPanel, BorderLayout.CENTER);
    }

    private void initEvents () {
        addPropertyChangeListener(this);
        getModel().addListDataListener(this);
    }

    @Override
    public int locationToIndex (Point p) {
        ChartRenderingInfo rInfo = mChartPanel.getChartRenderingInfo();
        ChartEntity entity = mChartPanel.getEntityForPoint(p.x, p.y);

        if (entity instanceof XYItemEntity == false)
            return -1;

        IParameterInfo pInfo = getParameterInfo();
        int index = ((XYItemEntity)entity).getItem();
        return index;
    }

    @Override
    public Map<Integer, Number> getValuesAt (Point p) {
        int index = locationToIndex(p);
        if (index == -1)
            return null;

        Number[] values = ((CombinationModel)getModel()).getValues(index);
        HashMap<Integer, Number> ret = new HashMap<>();
        for (int i=0; i<values.length; i++)
            ret.put(i, values[i]);

        return ret;
    }

    @Override
    public Map<Integer, Number> getValuesAtHover (Point p) {
        return getValuesAt(p);
    }

    public void onCancelled (int seriesIndex, int index) {
        XYSeriesCollection dataset = (XYSeriesCollection)mChart.getXYPlot().getDataset(seriesIndex);
        ((XYProgressSeries)dataset.getSeries(seriesIndex)).onCancelled(index);
    }

    @Override
    public void ensureIndexIsVisible (int index) {
        XYSeriesCollection dataset = (XYSeriesCollection)mChart.getXYPlot().getDataset(0);
        XYSeries series = dataset.getSeries(0);
    }

    @Override
    public boolean getValueIsAdjusting (int index) {
        return getValueIsAdjusting(0, index);
    }

    public boolean getValueIsAdjusting (int seriesIndex, int index) {
        XYSeriesCollection dataset = (XYSeriesCollection)mChart.getXYPlot().getDataset(seriesIndex);
        return ((XYProgressSeries)dataset.getSeries(seriesIndex)).getValueIsAdjustring(index);
    }

    @Override
    public void setValueIsAdjusting (int index, boolean isAdjusting) {
        setValueIsAdjusting(0, index, isAdjusting);
    }

    public void setValueIsAdjusting (int seriesIndex, int index, boolean isAdjusting) {
        XYSeriesCollection dataset = (XYSeriesCollection)mChart.getXYPlot().getDataset(seriesIndex);
        ((XYProgressSeries)dataset.getSeries(seriesIndex)).setValueIsAdjusting(index, isAdjusting);
    }

    @Override
    public void setCancelled (int index, boolean cancelled) {
        setCancelled(0, index, cancelled);
    }

    public void setCancelled (int seriesIndex, int index, boolean cancelled) {
        XYSeriesCollection dataset = (XYSeriesCollection)mChart.getXYPlot().getDataset(seriesIndex);
        ((XYProgressSeries)dataset.getSeries(seriesIndex)).setCancelled(index, cancelled);
    }

    @Override
    public void setProgress (int index, int progress) {
        setProgress(0, index, progress);
    }

    public void setProgress (int seriesIndex, int index, int progress) {
        XYSeriesCollection dataset = (XYSeriesCollection)mChart.getXYPlot().getDataset(seriesIndex);
        ((XYProgressSeries)dataset.getSeries(seriesIndex)).setProgress(index, progress);
    }

    @Override
    public int getProgress (int index) {
        return getProgress(0, index);
    }

    public int getProgress (int seriesIndex, int index) {
        XYSeriesCollection dataset = (XYSeriesCollection)mChart.getXYPlot().getDataset(seriesIndex);
        return ((XYProgressSeries)dataset.getSeries(seriesIndex)).getProgress(index);
    }

    public boolean isCancelled (int seriesIndex, int index) {
        XYSeriesCollection dataset = (XYSeriesCollection)mChart.getXYPlot().getDataset(seriesIndex);
        return ((XYProgressSeries)dataset.getSeries(seriesIndex)).isCancelled(index);
    }

    @Override
    public boolean isCancelled (int index) {
        return isCancelled(0, index);
    }

    @Override
    public void intervalAdded (ListDataEvent evt) {
        int start = evt.getIndex0();
        int end   = evt.getIndex1();

        if (start < 0)
            return;

        XYSeriesCollection dataset = (XYSeriesCollection)mChart.getXYPlot().getDataset(0);
        XYSeries series = dataset.getSeries(0);

        for (int i=start; i<=end; i++) {
            Number[] data = (Number[])getModel().getElementAt(i);
            if (getActiveParameterCount() == 1) {
                series.add(data[0], 1, true);
            } else if (getActiveParameterCount() == 2) {
                series.add(data[0], data[1], true);
            }
        }
        series.fireSeriesChanged();
    }

    @Override
    public void intervalRemoved (ListDataEvent evt) {
        int start = evt.getIndex0();
        int end   = evt.getIndex1();

        XYSeriesCollection dataset = (XYSeriesCollection)mChart.getXYPlot().getDataset(0);
        XYSeries series = dataset.getSeries(0);
        series.delete(start, end);
    }

    @Override
    public void contentsChanged (ListDataEvent evt) {
        int start = evt.getIndex0();
        int end   = evt.getIndex1();

        XYSeriesCollection dataset = (XYSeriesCollection)mChart.getXYPlot().getDataset(0);
        XYSeries series = dataset.getSeries(0);

        series.delete(start, end);

        for (int i=start; i<=end; i++) {
            Number[] data = (Number[])getModel().getElementAt(i);
            series.add(data[0], data[1], true);
        }
        series.fireSeriesChanged();
    }

    @Override
    public void propertyChange(PropertyChangeEvent evt) {
        String propertyName = evt.getPropertyName();
        Object newValue = evt.getNewValue();
        Object oldValue = evt.getOldValue();

        if ("model".equals(propertyName)) {
            if (oldValue instanceof ListModel)
                ((ListModel)oldValue).removeListDataListener(this);

            if (newValue instanceof ListModel)
                ((ListModel)newValue).addListDataListener(this);

            if (mChart == null) return;
            XYSeriesCollection dataset = (XYSeriesCollection)mChart.getXYPlot().getDataset(0);
            XYSeries series = dataset.getSeries(0);
            ListModel model = getModel();
            series.clear();
            ListDataEvent lde = new ListDataEvent(model, 
                    ListDataEvent.INTERVAL_ADDED, 0, model.getSize()-1);
            intervalAdded(lde);
        } else if ("parameterInfo".equals(propertyName)) {
            XYPlot plot = (XYPlot)mChart.getPlot();
            NumberAxis xAxis = (NumberAxis) plot.getDomainAxis();
            NumberAxis yAxis = (NumberAxis) plot.getRangeAxis();
            if (getActiveParameterCount() == 1) {
                yAxis.setNumberFormatOverride(new EmptyNumberFormat());
                TickUnits tickUnits = new TickUnits();
                TickUnit unit = new NumberTickUnit(1);
                tickUnits.add(unit);
                yAxis.setStandardTickUnits(tickUnits);
            } else if (getActiveParameterCount() == 2) {
                xAxis.setAttributedLabel(getParameterInfo().getTitle(0));
                yAxis.setAttributedLabel(getParameterInfo().getTitle(1));
                ((NumberAxis) yAxis).setNumberFormatOverride(null);
            }
        } else if ("background".equals(propertyName)) {
            XYPlot plot = (XYPlot)mChart.getPlot();
            plot.setBackgroundPaint(getBackground());
        } else if ("foreground".equals(propertyName)) {
            XYPlot plot = (XYPlot)mChart.getPlot();
            plot.setRangeGridlinePaint(getForeground());
            plot.setDomainGridlinePaint(getForeground());
        }
    }

    private class ChartPanelEventHandler implements MouseListener, MouseMotionListener {
        @Override
        public void mouseClicked(MouseEvent evt) {
            dispatchEvent(transformPoint(evt));
        }

        @Override
        public void mousePressed(MouseEvent evt) {
            dispatchEvent(transformPoint(evt));
        }

        @Override
        public void mouseReleased(MouseEvent evt) {
            dispatchEvent(transformPoint(evt));
        }

        @Override
        public void mouseEntered(MouseEvent evt) {
            dispatchEvent(transformPoint(evt));
        }

        @Override
        public void mouseExited(MouseEvent evt) {
            dispatchEvent(transformPoint(evt));
        }

        @Override
        public void mouseDragged(MouseEvent evt) {
            dispatchEvent(transformPoint(evt));
        }

        @Override
        public void mouseMoved(MouseEvent evt) {
            dispatchEvent(transformPoint(evt));
            Point p = evt.getPoint();
            int index = locationToIndex(p);

            if (index == -1)
                getSelectionModel().clearSelection();

            getSelectionModel().setSelectionInterval(index, index);
        }
    }

    private static class EmptyNumberFormat extends NumberFormat {
        @Override
        public StringBuffer format(double d, StringBuffer sb, FieldPosition fp) {
            return new StringBuffer();
        }

        @Override
        public StringBuffer format(long l, StringBuffer sb, FieldPosition fp) {
            return new StringBuffer();
        }

        @Override
        public Number parse(String string, ParsePosition pp) {
            return null;
        }
    }
}
