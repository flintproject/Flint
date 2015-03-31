/* -*- Mode: Java; tab-width: 4; indent-tabs-mode: nil; c-basic-offset: 4 -*- vim:set ts=4 sw=4 sts=4 et: */
package jp.oist.flint.view;

import jp.oist.flint.form.sub.IChartController;
import org.jfree.chart.JFreeChart;
import java.beans.PropertyChangeEvent;
import java.beans.PropertyChangeListener;
import java.util.concurrent.ExecutionException;
import java.util.logging.Level;
import java.util.logging.Logger;
import javax.swing.SwingWorker;
import org.jfree.chart.ChartPanel;

public class ChartPainter implements PropertyChangeListener {

    private final IChartController mController;
    private final ChartRenderer mRenderer;

    public ChartPainter(IChartController controller, ChartRenderer renderer) {
        mController = controller;
        mRenderer = renderer;
    }

    @Override
    public void propertyChange(PropertyChangeEvent evt) {
        String propertyName = evt.getPropertyName();
        Object nv = evt.getNewValue();
        if ("state".equals(propertyName)
            && nv == SwingWorker.StateValue.DONE) {
            JFreeChart chart;
            try {
                chart = mRenderer.get();
            } catch (InterruptedException | ExecutionException ex) {
                Logger.getLogger(ChartPainter.class.getName()).log(Level.SEVERE, null, ex);
                return;
            }
            ChartPanel panel = new ChartPanel(chart);
            mController.displayChart(panel);
        }
    }
}
