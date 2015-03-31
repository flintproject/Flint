/* -*- Mode: Java; tab-width: 4; indent-tabs-mode: nil; c-basic-offset: 4 -*- vim:set ts=4 sw=4 sts=4 et: */
package jp.oist.flint.view;

import org.jfree.chart.ChartFactory;
import org.jfree.chart.JFreeChart;
import org.jfree.chart.axis.AxisLocation;
import org.jfree.chart.axis.LogAxis;
import org.jfree.chart.axis.NumberAxis;
import org.jfree.chart.plot.XYPlot;
import org.jfree.chart.renderer.xy.StandardXYItemRenderer;
import org.jfree.data.xy.DefaultXYDataset;
import org.jfree.data.xy.XYDataset;
import org.jfree.data.xy.XYSeries;
import org.jfree.data.xy.XYSeriesCollection;
import java.util.ArrayList;
import javax.swing.SwingWorker;

public class ChartRenderer extends SwingWorker<JFreeChart, Void> {

    private final ArrayList<XYSeries> mData;
    private final ChartConfig mConfig;

    public ChartRenderer(ArrayList<XYSeries> data, ChartConfig config) {
        mData = data;
        mConfig = config;
    }

    @Override
    protected JFreeChart doInBackground() {
        JFreeChart chart;
        int y1size = mConfig.getY1indices().size();
        int y2size = mConfig.getY2indices().size();
        if (mConfig.getXindex() == 0) {
            XYSeriesCollection dataset1 = new XYSeriesCollection();
            for (int i = 0; i < y1size; i++) {
                dataset1.addSeries(mData.get(i+1));
            }
            if (y2size == 0) {
                chart = createChart(mConfig.getXlog(),
                                    dataset1, mConfig.getY1log());
            } else {
                XYSeriesCollection dataset2 = new XYSeriesCollection();
                for (int i = 0; i < y2size; i++) {
                    dataset2.addSeries(mData.get(i+1+y1size));
                }
                chart = createChart(mConfig.getXlog(),
                                    dataset1, mConfig.getY1log(),
                                    dataset2, mConfig.getY2log());
            }
        } else {
            double[][] x = mData.get(0).toArray();

            DefaultXYDataset dataset1 = new DefaultXYDataset();
            for (int i = 0; i < y1size; i++) {
                double[][] y = mData.get(i+1).toArray();
                y[0] = x[1];
                dataset1.addSeries(mConfig.getY1legend(i), y);
            }
            if (y2size == 0) {
                chart = createChart(mConfig.getXlog(),
                                    dataset1, mConfig.getY1log());
            } else {
                DefaultXYDataset dataset2 = new DefaultXYDataset();
                for (int i = 0; i < y2size; i++) {
                    double[][] y = mData.get(i+1+y1size).toArray();
                    y[0] = x[1];
                    dataset2.addSeries(mConfig.getY2legend(i), y);
                }
                chart = createChart(mConfig.getXlog(),
                                    dataset1, mConfig.getY1log(),
                                    dataset2, mConfig.getY2log());
            }
        }
        return chart;
    }

    private JFreeChart createChart(boolean xLog, XYDataset dataset, boolean yLog) {
        JFreeChart chart = ChartFactory.createXYLineChart(null,
                                                          null,
                                                          null,
                                                          dataset,
                                                          org.jfree.chart.plot.PlotOrientation.VERTICAL,
                                                          mConfig.getLegend(),
                                                          true,
                                                          false);
        XYPlot plot = chart.getXYPlot();
        if (xLog) {
            LogAxis axis = new LogAxis();
            plot.setDomainAxis(axis);
        }
        if (yLog) {
            LogAxis axis = new LogAxis();
            plot.setRangeAxis(0, axis);
        }
        return chart;
    }

    private JFreeChart createChart(boolean xLog,
                                   XYDataset dataset, boolean yLog,
                                   XYDataset dataset2, boolean y2Log) {
        JFreeChart chart = createChart(xLog, dataset, yLog);
        XYPlot plot = chart.getXYPlot();
        if (y2Log) {
            LogAxis axis = new LogAxis();
            plot.setRangeAxis(1, axis);
        } else {
            NumberAxis axis = new NumberAxis();
            plot.setRangeAxis(1, axis);
        }
        plot.setRangeAxisLocation(1, AxisLocation.TOP_OR_RIGHT);
        plot.setDataset(1, dataset2);
        plot.mapDatasetToRangeAxis(1, 1);
        StandardXYItemRenderer renderer = new StandardXYItemRenderer();
        plot.setRenderer(1, renderer);
        return chart;
    }

}
