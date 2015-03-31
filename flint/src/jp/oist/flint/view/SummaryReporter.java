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


public class SummaryReporter implements PropertyChangeListener {

    private final IChartController mController;
    private final TimeSeriesReader mReader;
    private final String mName;

    public SummaryReporter(IChartController controller, TimeSeriesReader reader, String name) {
        mController = controller;
        mReader = reader;
        mName = name;
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
            SummaryPainter painter = new SummaryPainter(mController, result.get(0), mName);
            painter.execute();
        }
    }

}
