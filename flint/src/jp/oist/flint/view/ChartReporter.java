/* -*- Mode: Java; tab-width: 4; indent-tabs-mode: nil; c-basic-offset: 4 -*- vim:set ts=4 sw=4 sts=4 et: */
package jp.oist.flint.view;

import jp.oist.flint.form.sub.IChartController;
import org.jfree.data.xy.XYSeries;
import java.beans.PropertyChangeEvent;
import java.beans.PropertyChangeListener;
import java.util.ArrayList;
import java.util.concurrent.ExecutionException;
import java.util.logging.Level;
import java.util.logging.Logger;
import javax.swing.SwingWorker;


public class ChartReporter implements PropertyChangeListener {

    private final IChartController mController;
    private final TimeSeriesReader mReader;
    private final ChartConfig mConfig;

    public ChartReporter(IChartController controller, TimeSeriesReader reader, ChartConfig config) {
        mController = controller;
        mReader = reader;
        mConfig = config;
    }

    @Override
    public void propertyChange(PropertyChangeEvent evt) {
        String propertyName = evt.getPropertyName();
        Object nv = evt.getNewValue();
        if ("state".equals(propertyName)
            && nv == SwingWorker.StateValue.DONE) {
            ArrayList<XYSeries> result;
            try {
                result = mReader.get();
            } catch (InterruptedException | ExecutionException ex) {
                Logger.getLogger(SummaryReporter.class.getName()).log(Level.SEVERE, null, ex);
                return;
            }
            ChartRenderer renderer = new ChartRenderer(result, mConfig);
            renderer.addPropertyChangeListener(new ChartPainter(mController, renderer));
            renderer.execute();
        }
    }
}
