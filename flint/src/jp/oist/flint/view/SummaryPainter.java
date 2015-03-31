/* -*- Mode: Java; tab-width: 4; indent-tabs-mode: nil; c-basic-offset: 4 -*- vim:set ts=4 sw=4 sts=4 et: */
package jp.oist.flint.view;

import jp.oist.flint.form.sub.IChartController;
import org.jfree.data.xy.XYSeries;
import java.util.concurrent.ExecutionException;
import java.util.logging.Level;
import java.util.logging.Logger;
import javax.swing.SwingWorker;

public class SummaryPainter extends SwingWorker<Summary, Void> {

    private final IChartController mController;
    private final XYSeries mTimeSeries;
    private final String mName;

    public SummaryPainter(IChartController controller, XYSeries timeSeries, String name) {
        mController = controller;
        mTimeSeries = timeSeries;
        mName = name;
    }

    @Override
    protected Summary doInBackground() {
        return new Summary(mName, mTimeSeries.getMaxY(), mTimeSeries.getMinY());
    }

    @Override
    protected void done() {
        Summary summary;
        try {
            summary = get();
        } catch (InterruptedException | ExecutionException ex) {
            Logger.getLogger(SummaryPainter.class.getName()).log(Level.SEVERE, null, ex);
            return;
        }
        mController.displaySummary(summary);
    }
}
